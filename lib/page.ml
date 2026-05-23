open! Core
open! Async

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

let wait_for_load t =
  let events = Connection.events t.connection in
  Deferred.repeat_until_finished () (fun () ->
    match%map Pipe.read events with
    | `Eof -> `Finished ()
    | `Ok { Connection.Event.method_name; session_id; params = _ } ->
      let is_load_for_session =
        String.equal method_name Protocol.Page.load_event_fired_method_name
        && Option.equal String.equal session_id (Some t.session_id)
      in
      (match is_load_for_session with
       | true -> `Finished ()
       | false -> `Repeat ()))
;;

let navigate ?(load_timeout = Time_ns.Span.of_int_sec 30) t ~url =
  let do_navigate =
    let%bind.Deferred.Or_error (_ : Protocol.Page.Navigate.Result.t) =
      call t Protocol.Page.Navigate.method_ { url }
    in
    let%map.Deferred () = wait_for_load t in
    Ok ()
  in
  match%map.Deferred Clock_ns.with_timeout load_timeout do_navigate with
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
