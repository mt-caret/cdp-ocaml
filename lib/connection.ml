open! Core
open! Async
open Jsonaf.Export

module Event = struct
  type t =
    { method_name : string
    ; params : Jsonaf.t option
    }

  module Kind = struct
    module T = struct
      type t =
        [ `Session_id of string
        | `No_session_id
        ]
      [@@deriving compare, hash, sexp_of]
    end

    include T
    include Hashable.Make_plain (T)
  end
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
      ; params : Jsonaf.t
      }
    [@@deriving jsonaf_of]
  end

  module Incoming = struct
    type t =
      { id : int option [@jsonaf.option]
      ; result : Jsonaf.t option [@jsonaf.option]
      ; error : Error_body.t option [@jsonaf.option]
      ; method_ : string option [@key "method"] [@jsonaf.option]
      ; session_id : string option [@key "sessionId"] [@jsonaf.option]
      ; params : Jsonaf.t option [@jsonaf.option]
      }
    [@@deriving of_jsonaf] [@@allow_extra_fields]
  end
end

type t =
  { send_pipe : string Pipe.Writer.t
  ; subscribers : Event.t Pipe.Writer.t Bag.t Event.Kind.Table.t
  ; next_id : int ref
  ; pending : Jsonaf.t Or_error.t Ivar.t Int.Table.t
  }

let process_message ~subscribers ~pending s =
  [%log.debug "cdp-recv" ~json:s];
  match Jsonaf.parse s with
  | Error err -> [%log.error "cdp-recv-parse-failed" (err : Error.t) ~raw:s]
  | Ok json ->
    let%tydi { id; result; error; method_; session_id; params } =
      [%of_jsonaf: Wire.Incoming.t] json
    in
    (match id, method_ with
     | None, None -> [%log.debug "cdp-recv-no-id-or-method" ~json:s]
     | None, Some method_name ->
       let kind =
         match session_id with
         | Some session_id -> `Session_id session_id
         | None -> `No_session_id
       in
       (match Hashtbl.find subscribers kind with
        | None -> [%log.debug "cdp-recv-no-subscribers" ~json:s]
        | Some writers ->
          (* TODO: Is error ever populated here? If so, subscribers should be
 .           notified of them. *)
          Bag.iter writers ~f:(fun writer ->
            Pipe.write_without_pushback_if_open writer { Event.method_name; params }))
     | Some id, _ ->
       (match Hashtbl.find_and_remove pending id with
        | None -> [%log.error "cdp-recv-unknown-id" (id : int) ~raw:s]
        | Some ivar ->
          let result =
            match error with
            | Some err ->
              Or_error.error_s [%message "CDP error" ~_:(err : Wire.Error_body.t)]
            | None -> Ok (Option.value result ~default:(`Object []))
          in
          Ivar.fill_exn ivar result))
;;

let connect uri =
  let open Deferred.Or_error.Let_syntax in
  let%map _response, ws = Cohttp_async_websocket.Client.create uri in
  let recv_pipe, send_pipe = Websocket.pipes ws in
  let subscribers = Event.Kind.Table.create () in
  let pending = Int.Table.create () in
  don't_wait_for
    (let%bind.Deferred () =
       Pipe.iter_without_pushback recv_pipe ~f:(process_message ~subscribers ~pending)
     in
     Hashtbl.iter subscribers ~f:(fun writers -> Bag.iter writers ~f:Pipe.close);
     (* Fail all pending requests. [Ivar.fill_exn] is safe: each id maps to one ivar
        that is filled exactly once, here or in [process_message]. *)
     Hashtbl.iter pending ~f:(fun ivar ->
       Ivar.fill_exn ivar (Or_error.error_string "CDP connection closed before response"));
     Hashtbl.clear pending;
     Deferred.unit);
  { send_pipe; subscribers; next_id = ref 0; pending }
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
    [%log.debug "cdp-send" ~json:envelope_str];
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

let subscribe t kind =
  let reader, writer = Pipe.create () in
  let bag = Hashtbl.find_or_add t.subscribers kind ~default:(fun () -> Bag.create ()) in
  let elt = Bag.add bag writer in
  don't_wait_for
    (let%map () = Pipe.closed reader in
     Bag.remove bag elt;
     if Bag.is_empty bag then Hashtbl.remove t.subscribers kind);
  reader
;;

let events_for_session t ~session_id = subscribe t (`Session_id session_id)
let events_without_session t = subscribe t `No_session_id

let close t =
  Pipe.close t.send_pipe;
  Pipe.closed t.send_pipe
;;
