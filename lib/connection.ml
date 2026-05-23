open! Core
open! Async
open Jsonaf.Export

module Json = struct
  type t = Jsonaf.t

  let jsonaf_of_t (x : t) = x
  let t_of_jsonaf (x : t) = x
end

module Event = struct
  type t =
    { method_name : string
    ; session_id : string option
    ; params : Jsonaf.t option
    }
end

module Wire = struct
  module Error_body = struct
    type t =
      { code : int
      ; message : string
      }
    [@@deriving of_jsonaf, sexp_of] [@@allow_extra_fields]
  end

  module Request = struct
    type t =
      { id : int
      ; session_id : string option [@key "sessionId"] [@jsonaf.option]
      ; method_ : string [@key "method"]
      ; params : Json.t
      }
    [@@deriving jsonaf_of]
  end

  module Incoming = struct
    type t =
      { id : int option [@jsonaf.option]
      ; result : Json.t option [@jsonaf.option]
      ; error : Error_body.t option [@jsonaf.option]
      ; method_ : string option [@key "method"] [@jsonaf.option]
      ; session_id : string option [@key "sessionId"] [@jsonaf.option]
      ; params : Json.t option [@jsonaf.option]
      }
    [@@deriving of_jsonaf] [@@allow_extra_fields]
  end
end

type t =
  { send_pipe : string Pipe.Writer.t
  ; events_r : Event.t Pipe.Reader.t
  ; next_id : int ref
  ; pending : Jsonaf.t Or_error.t Ivar.t Int.Table.t
  }

let events t = t.events_r

let process_message ~pending s =
  [%log.global.debug "cdp-recv" ~json:s];
  match Jsonaf.parse s with
  | Error err ->
    [%log.global.error "cdp-recv-parse-failed" (err : Error.t) ~raw:s];
    None
  | Ok json ->
    let%tydi { id; result; error; method_; session_id; params } =
      [%of_jsonaf: Wire.Incoming.t] json
    in
    (match id, method_ with
     | None, Some method_name -> Some { Event.method_name; session_id; params }
     | Some id, _ ->
       (match Hashtbl.find_and_remove pending id with
        | None ->
          [%log.global.error "cdp-recv-unknown-id" (id : int) ~raw:s];
          ()
        | Some ivar ->
          let result =
            match error with
            | Some err ->
              Or_error.error_s [%message "CDP error" ~_:(err : Wire.Error_body.t)]
            | None -> Ok (Option.value result ~default:(`Object []))
          in
          Ivar.fill_exn ivar result);
       None
     | None, None -> None)
;;

let fail_pending pending error =
  Hashtbl.iter pending ~f:(fun ivar -> Ivar.fill_if_empty ivar (Error error));
  Hashtbl.clear pending
;;

let connect uri =
  let open Deferred.Or_error.Let_syntax in
  let%map _response, ws = Cohttp_async_websocket.Client.create uri in
  let recv_pipe, send_pipe = Websocket.pipes ws in
  let events_r, events_w = Pipe.create () in
  let pending = Int.Table.create () in
  don't_wait_for
    (let%bind.Deferred () =
       Pipe.iter_without_pushback recv_pipe ~f:(fun s ->
         match process_message ~pending s with
         | None -> ()
         | Some event -> Pipe.write_without_pushback_if_open events_w event)
     in
     Pipe.close events_w;
     (* Fail all pending requests *)
     Hashtbl.iter pending ~f:(fun ivar ->
       (* [Ivar.fill_exn] should never raise, since [pending] only contain ivars
          that are waiting for a response and no async cycles run between
          deferred binds. *)
       Ivar.fill_exn ivar (Or_error.error_string "CDP connection closed before response"));
     Hashtbl.clear pending;
     Deferred.unit);
  { send_pipe; events_r; next_id = ref 0; pending }
;;

let connect_exn uri = connect uri >>| ok_exn

let call_raw t ?session_id ~method_ ~params () =
  match Pipe.is_closed t.send_pipe with
  | true -> Deferred.Or_error.error_s [%message "CDP send pipe closed" (method_ : string)]
  | false ->
    let id = !(t.next_id) in
    incr t.next_id;
    let envelope_str =
      [%jsonaf_of: Wire.Request.t] { id; session_id; method_; params } |> Jsonaf.to_string
    in
    [%log.global.debug "cdp-send" ~json:envelope_str];
    let ivar = Ivar.create () in
    Hashtbl.set t.pending ~key:id ~data:ivar;
    let%bind () = Pipe.write t.send_pipe envelope_str in
    Ivar.read ivar
;;

let call_raw_exn t ?session_id ~method_ ~params () =
  call_raw t ?session_id ~method_ ~params () >>| ok_exn
;;

let call_with_scope
      t
      ?session_id
      { Method.name; params_to_jsonaf; result_of_jsonaf }
      params
  =
  let%bind.Deferred.Or_error raw =
    call_raw t ?session_id ~method_:name ~params:(params_to_jsonaf params) ()
  in
  return
    (Or_error.try_with (fun () -> result_of_jsonaf raw)
     |> Or_error.tag ~tag:[%string "CDP response decode failed for %{name}"])
;;

let call t (method_ : (_, _, [ `Browser ]) Method.t) params =
  call_with_scope t method_ params
;;

let call_exn t method_ params = call t method_ params >>| ok_exn

let call_page t ~session_id (method_ : (_, _, [ `Page ]) Method.t) params =
  call_with_scope t ~session_id method_ params
;;

let call_page_exn t ~session_id method_ params =
  call_page t ~session_id method_ params >>| ok_exn
;;

let close t =
  Pipe.close t.send_pipe;
  Pipe.closed t.send_pipe
;;
