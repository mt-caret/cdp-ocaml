open! Core
open! Async

type t =
  { process : Process.t
  ; user_data_dir : string
  ; connection : Connection.t
  }

let read_port_file path =
  let max_attempts = 200 in
  let delay = Time_ns.Span.of_int_ms 50 in
  let rec poll n =
    match%bind Sys.file_exists path with
    | `Yes ->
      let%bind contents = Reader.file_contents path in
      (match String.split_lines contents with
       | port_str :: ws_path :: _ ->
         (match Int.of_string_opt (String.strip port_str) with
          | Some port -> return (Ok (port, String.strip ws_path))
          | None ->
            return
              (Or_error.error_s
                 [%message "Bad port in DevToolsActivePort" (port_str : string)]))
       | _ ->
         return
           (Or_error.error_s
              [%message "DevToolsActivePort missing ws path" (contents : string)]))
    | `No | `Unknown ->
      (match n with
       | 0 ->
         return
           (Or_error.error_s
              [%message "Timed out waiting for DevToolsActivePort" (path : string)])
       | _ ->
         let%bind () = Clock_ns.after delay in
         poll (n - 1))
  in
  poll max_attempts
;;

let launch ?chrome_path ?(extra_args = []) () =
  let open Deferred.Or_error.Let_syntax in
  let%bind chrome_path =
    match chrome_path with
    | Some p -> return p
    | None -> Chrome_path.find ()
  in
  let user_data_dir =
    Core_unix.mkdtemp (Filename.temp_dir_name ^/ "cdp-profile-XXXXXX")
  in
  let default_args =
    [ "--headless=new"
    ; "--no-sandbox"
    ; "--disable-gpu"
    ; "--hide-scrollbars"
    ; "--mute-audio"
    ; "--no-first-run"
    ; "--no-default-browser-check"
    ; "--remote-debugging-port=0"
    ; "--remote-allow-origins=*"
    ; [%string "--user-data-dir=%{user_data_dir}"]
    ; "about:blank"
    ]
  in
  let%bind process =
    Process.create ~prog:chrome_path ~args:(default_args @ extra_args) ()
  in
  don't_wait_for (Pipe.drain (Reader.pipe (Process.stdout process)));
  don't_wait_for (Pipe.drain (Reader.pipe (Process.stderr process)));
  let port_file = user_data_dir ^/ "DevToolsActivePort" in
  let%bind port, path = read_port_file port_file in
  let ws_url = Uri.make () ~scheme:"ws" ~host:"127.0.0.1" ~port ~path in
  let%map connection = Connection.connect ws_url in
  { process; user_data_dir; connection }
;;

let launch_exn ?chrome_path ?extra_args () = launch ?chrome_path ?extra_args () >>| ok_exn
let connection t = t.connection

let remove_user_data_dir t =
  match%map
    Monitor.try_with ~rest:`Log (fun () ->
      Process.run_expect_no_output_exn ~prog:"rm" ~args:[ "-rf"; t.user_data_dir ] ())
  with
  | Ok () | Error _ -> ()
;;

let send_cdp_close t =
  (* Cap the wait for chromium's reply: if it's unresponsive we want to fall through to
     the signal-based escalation in [close] rather than block here. *)
  Clock_ns.with_timeout
    (Time_ns.Span.of_int_sec 2)
    (Monitor.try_with ~rest:`Log (fun () ->
       Connection.call t.connection Protocol.Browser.Close.method_ ()))
  |> Deferred.ignore_m
;;

let close
      ?(graceful_timeout = Time_ns.Span.of_int_sec 5)
      ?(term_timeout = Time_ns.Span.of_int_sec 5)
      t
  =
  let exited = Process.wait t.process in
  let%bind () = send_cdp_close t in
  let%bind () = Connection.close t.connection in
  let%bind () =
    match%bind Clock_ns.with_timeout graceful_timeout exited with
    | `Result _ -> return ()
    | `Timeout ->
      Process.send_signal t.process Core.Signal.term;
      (match%map Clock_ns.with_timeout term_timeout exited with
       | `Result _ -> ()
       | `Timeout ->
         Process.send_signal t.process Core.Signal.kill;
         don't_wait_for (Deferred.ignore_m exited))
  in
  remove_user_data_dir t
;;

let with_browser ?chrome_path ?extra_args ~f () =
  match%bind launch ?chrome_path ?extra_args () with
  | Error _ as e -> return e
  | Ok browser -> Monitor.protect (fun () -> f browser) ~finally:(fun () -> close browser)
;;

let with_browser_exn ?chrome_path ?extra_args ~f () =
  let%bind browser = launch_exn ?chrome_path ?extra_args () in
  Monitor.protect (fun () -> f browser) ~finally:(fun () -> close browser)
;;
