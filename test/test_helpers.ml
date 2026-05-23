open! Core
open! Async

(* Pin Log.Global's time source to [Time_ns.epoch] so log messages (e.g. the "Websocket
   closed" line emitted on browser teardown) have a deterministic timestamp in expect
   tests. The default output and UTC zone are fine. *)
let () =
  Synchronous_time_source.create ~now:Time_ns.epoch ()
  |> Synchronous_time_source.read_only
  |> Log.Global.set_time_source
;;

let with_server ~routes ~f =
  let routes = String.Map.of_alist_exn routes in
  let handler ~body:_ _addr req =
    let path = Uri.path (Cohttp.Request.uri req) in
    match Map.find routes path with
    | Some body -> Cohttp_async.Server.respond_string ~status:`OK body
    | None -> Cohttp_async.Server.respond_string ~status:`Not_found "not found"
  in
  let%bind server =
    Cohttp_async.Server.create
      ~on_handler_error:`Raise
      Tcp.Where_to_listen.of_port_chosen_by_os
      handler
  in
  let port = Cohttp_async.Server.listening_on server in
  Monitor.protect
    (fun () -> f ~port)
    ~finally:(fun () -> Cohttp_async.Server.close server)
;;

let with_hanging_server ~f =
  let%bind server =
    Cohttp_async.Server.create
      ~on_handler_error:`Raise
      Tcp.Where_to_listen.of_port_chosen_by_os
      (fun ~body:_ _addr _req -> Deferred.never ())
  in
  let port = Cohttp_async.Server.listening_on server in
  Monitor.protect
    (fun () -> f ~port)
    ~finally:(fun () -> Cohttp_async.Server.close server)
;;

(* Browser teardown, after [f] returns, emits the trailing "Websocket closed" log line
   seen in the expect blocks. *)
let with_page_on_html ~html ~f =
  with_server
    ~routes:[ "/", html ]
    ~f:(fun ~port ->
      Cdp.Browser.with_browser_exn
        ~f:(fun browser ->
          Cdp.Page.with_page_exn (Cdp.Browser.connection browser) ~f:(fun page ->
            let%bind () =
              Cdp.Page.navigate_exn page ~url:[%string "http://127.0.0.1:%{port#Int}/"]
            in
            f page))
        ())
;;
