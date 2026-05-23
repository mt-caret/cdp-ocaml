open! Core
open! Async
module Event_type = Protocol.Input.Dispatch_key_event.Event_type

type t =
  { connection : Connection.t
  ; target_id : string
  ; session_id : string
  }
[@@deriving fields ~getters]

let call t method_ params =
  Connection.call_page t.connection ~session_id:t.session_id method_ params
;;

let create connection =
  let open Deferred.Or_error.Let_syntax in
  let%bind { target_id } =
    Connection.call
      connection
      Protocol.Target.Create_target.method_
      { url = "about:blank" }
  in
  let%bind { session_id } =
    Connection.call
      connection
      Protocol.Target.Attach_to_target.method_
      { target_id; flatten = true }
  in
  let%map () =
    Connection.call_page connection ~session_id Protocol.Page.Enable.method_ ()
  in
  { connection; target_id; session_id }
;;

let create_exn connection = create connection >>| ok_exn

let wait_for_load events =
  Deferred.repeat_until_finished () (fun () ->
    match%map Pipe.read events with
    | `Eof -> `Finished ()
    | `Ok { Connection.Event.method_name; params = _ } ->
      (match String.equal method_name Protocol.Page.load_event_fired_method_name with
       | true -> `Finished ()
       | false -> `Repeat ()))
;;

let navigate ?(load_timeout = Time_ns.Span.of_int_sec 30) t ~url =
  (* Subscribe before navigating so we don't miss the load event. *)
  let events = Connection.events_for_session t.connection ~session_id:t.session_id in
  let do_navigate =
    let%bind.Deferred.Or_error (_ : Protocol.Page.Navigate.Result.t) =
      call t Protocol.Page.Navigate.method_ { url }
    in
    wait_for_load events |> Deferred.ok
  in
  let%map.Deferred result = Clock_ns.with_timeout load_timeout do_navigate in
  Pipe.close_read events;
  match result with
  | `Result x -> x
  | `Timeout ->
    Or_error.error_s [%message "navigate: timed out" (load_timeout : Time_ns.Span.t)]
;;

let navigate_exn ?load_timeout t ~url = navigate ?load_timeout t ~url >>| ok_exn

let screenshot_png t =
  let%bind.Deferred.Or_error { data } =
    call t Protocol.Page.Capture_screenshot.method_ ()
  in
  return (Or_error.try_with (fun () -> Base64.decode_exn data))
;;

let screenshot_png_exn t = screenshot_png t >>| ok_exn

let close t =
  Connection.call
    t.connection
    Protocol.Target.Close_target.method_
    { target_id = t.target_id }
;;

let close_exn t = close t >>| ok_exn

let with_page connection ~f =
  match%bind create connection with
  | Error _ as e -> return e
  | Ok page ->
    Monitor.protect
      (fun () -> f page)
      ~finally:(fun () -> close page |> Deferred.ignore_m)
;;

let with_page_exn connection ~f =
  let%bind page = create_exn connection in
  Monitor.protect
    (fun () -> f page)
    ~finally:(fun () ->
      (* Cap cleanup: if the browser is hung (e.g. mid-navigation to an unresponsive
         server), don't block forever on [Target.closeTarget]. [Browser.close] will
         tear the whole process down anyway. *)
      Clock_ns.with_timeout (Time_ns.Span.of_int_sec 2) (close page) |> Deferred.ignore_m)
;;

(* High-level DOM layer. Keyboard and evaluation are inlined here (they only need [call]);
   element queries/actions live in {!Locator}, which is parameterized by a [Page.t]. *)

let eval (type a) t (witness : a Js_type.t) expression : a Deferred.Or_error.t =
  let%bind.Deferred.Or_error result =
    call
      t
      Protocol.Runtime.Evaluate.method_
      { expression; return_by_value = true; await_promise = true }
  in
  Deferred.return
    (match (result : Protocol.Runtime.Evaluate.Result.t) with
     | Returned remote_object -> Js_type.decode witness remote_object
     | Exception { text; description } ->
       Or_error.error_s
         [%message "JS evaluation raised" (text : string) (description : string option)])
;;

let eval_exn t witness expression = eval t witness expression >>| ok_exn
let title t = eval t Js_type.String "document.title"
let url t = eval t Js_type.String "location.href"
let content t = eval t Js_type.String "document.documentElement.outerHTML"

let press t ?(modifiers = Key.Modifier.empty) key =
  let dispatch event_type =
    call
      t
      Protocol.Input.Dispatch_key_event.method_
      (Key.dispatch_params key ~modifiers ~event_type)
  in
  let%bind.Deferred.Or_error () = dispatch Event_type.Key_down in
  dispatch Event_type.Key_up
;;

let type_text t text = call t Protocol.Input.Insert_text.method_ { text }
let title_exn t = title t >>| ok_exn
let url_exn t = url t >>| ok_exn
let content_exn t = content t >>| ok_exn
let press_exn t ?modifiers key = press t ?modifiers key >>| ok_exn
let type_text_exn t text = type_text t text >>| ok_exn
