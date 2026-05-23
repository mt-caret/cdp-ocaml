open! Core
open! Async
open Test_helpers

let%expect_test "Chrome_path.find succeeds on this machine" =
  let%bind (_ : string) = Cdp.Chrome_path.find () >>| ok_exn in
  [%expect {| |}];
  return ()
;;

let%expect_test "Browser launches and closes cleanly" =
  let%bind () = Cdp.Browser.with_browser_exn ~f:(fun _ -> return ()) () in
  [%expect
    {| 1970-01-01 00:00:00.000000Z ("Websocket closed"(reason Normal_closure)(msg"Pipe was closed")) |}];
  return ()
;;

let%expect_test "Page.screenshot returns valid PNG bytes" =
  let%bind () =
    Cdp.Browser.with_browser_exn
      ~f:(fun browser ->
        Cdp.Page.with_page_exn (Cdp.Browser.connection browser) ~f:(fun page ->
          let%map png = Cdp.Page.screenshot_png_exn page in
          (* Only the PNG signature + IHDR header is stable across Chrome builds; the
             compressed image data and total length vary, so hexdump just the prefix. *)
          print_endline (String.Hexdump.to_string_hum (String.prefix png 32));
          [%expect
            {|
            00000000  89 50 4e 47 0d 0a 1a 0a  00 00 00 0d 49 48 44 52  |.PNG........IHDR|
            00000010  00 00 03 20 00 00 02 58  08 02 00 00 00 15 14 15  |... ...X........|
            |}]))
      ()
  in
  [%expect
    {| 1970-01-01 00:00:00.000000Z ("Websocket closed"(reason Normal_closure)(msg"Pipe was closed")) |}];
  return ()
;;

let%expect_test "Page.navigate reaches a local cohttp-async server" =
  let%bind () =
    with_server
      ~routes:[ "/", "<html><head><title>T</title></head><body>Hello</body></html>" ]
      ~f:(fun ~port ->
        Cdp.Browser.with_browser_exn
          ~f:(fun browser ->
            Cdp.Page.with_page_exn (Cdp.Browser.connection browser) ~f:(fun page ->
              Cdp.Page.navigate_exn page ~url:[%string "http://127.0.0.1:%{port#Int}/"]))
          ())
  in
  [%expect
    {| 1970-01-01 00:00:00.000000Z ("Websocket closed"(reason Normal_closure)(msg"Pipe was closed")) |}];
  return ()
;;

let%expect_test "Connection.call_raw works for Browser.getVersion (escape hatch)" =
  let%bind () =
    Cdp.Browser.with_browser_exn
      ~f:(fun browser ->
        let%map json =
          Cdp.Connection.call_raw_exn
            (Cdp.Browser.connection browser)
            ~method_:"Browser.getVersion"
            ~params:(`Object [])
            ()
        in
        (* We don't print out the whole JSON out since the output is dependent
           on the version of Chrome. *)
        Jsonaf.assoc_list_exn json
        |> List.map ~f:fst
        |> List.sort ~compare:String.compare
        |> [%sexp_of: string list]
        |> print_s;
        [%expect {| (jsVersion product protocolVersion revision userAgent) |}])
      ()
  in
  [%expect
    {| 1970-01-01 00:00:00.000000Z ("Websocket closed"(reason Normal_closure)(msg"Pipe was closed")) |}];
  return ()
;;

let%expect_test "Unknown CDP method surfaces as Or_error" =
  let%bind () =
    Cdp.Browser.with_browser_exn
      ~f:(fun browser ->
        let%map () =
          Cdp.Connection.call_raw
            (Cdp.Browser.connection browser)
            ~method_:"NoSuchDomain.noSuchMethod"
            ~params:(`Object [])
            ()
          >>| [%sexp_of: Jsonaf.t Or_error.t]
          >>| print_s
        in
        [%expect
          {|
          (Error
           ("CDP error"
            ((code -32601) (message "'NoSuchDomain.noSuchMethod' wasn't found"))))
          |}])
      ()
  in
  [%expect
    {| 1970-01-01 00:00:00.000000Z ("Websocket closed"(reason Normal_closure)(msg"Pipe was closed")) |}];
  return ()
;;

let%expect_test "Multiple pages multiplex over one browser-level connection" =
  let%bind () =
    with_server
      ~routes:
        [ "/a", "<html><body>page A</body></html>"
        ; "/b", "<html><body>page B</body></html>"
        ]
      ~f:(fun ~port ->
        Cdp.Browser.with_browser_exn
          ~f:(fun browser ->
            let conn = Cdp.Browser.connection browser in
            let%bind page_a = Cdp.Page.create_exn conn in
            let%bind page_b = Cdp.Page.create_exn conn in
            let%bind () =
              Cdp.Page.navigate_exn page_a ~url:[%string "http://127.0.0.1:%{port#Int}/a"]
            in
            let%bind () =
              Cdp.Page.navigate_exn page_b ~url:[%string "http://127.0.0.1:%{port#Int}/b"]
            in
            let%bind png_a = Cdp.Page.screenshot_png_exn page_a in
            print_endline "page_a:";
            print_endline (String.Hexdump.to_string_hum (String.prefix png_a 32));
            [%expect
              {|
              page_a:
              00000000  89 50 4e 47 0d 0a 1a 0a  00 00 00 0d 49 48 44 52  |.PNG........IHDR|
              00000010  00 00 03 20 00 00 02 58  08 02 00 00 00 15 14 15  |... ...X........|
              |}];
            let%map png_b = Cdp.Page.screenshot_png_exn page_b in
            print_endline "page_b:";
            print_endline (String.Hexdump.to_string_hum (String.prefix png_b 32));
            [%expect
              {|
              page_b:
              00000000  89 50 4e 47 0d 0a 1a 0a  00 00 00 0d 49 48 44 52  |.PNG........IHDR|
              00000010  00 00 03 20 00 00 02 58  08 02 00 00 00 15 14 15  |... ...X........|
              |}])
          ())
  in
  [%expect
    {| 1970-01-01 00:00:00.000000Z ("Websocket closed"(reason Normal_closure)(msg"Pipe was closed")) |}];
  return ()
;;

let%expect_test "Page.navigate times out when the server hangs" =
  let%bind () =
    with_hanging_server ~f:(fun ~port ->
      Cdp.Browser.with_browser_exn
        ~f:(fun browser ->
          Cdp.Page.with_page_exn (Cdp.Browser.connection browser) ~f:(fun page ->
            Cdp.Page.navigate
              page
              ~url:[%string "http://127.0.0.1:%{port#Int}/"]
              ~load_timeout:(Time_ns.Span.of_int_ms 50)
            >>| [%sexp_of: unit Or_error.t]
            >>| print_s))
        ())
  in
  [%expect
    {|
    (Error ("navigate: timed out" (load_timeout 50ms)))
    1970-01-01 00:00:00.000000Z ("Websocket closed"(reason Normal_closure)(msg"Pipe was closed"))
    |}];
  return ()
;;

let%expect_test "Page.close succeeds without error" =
  let%bind () =
    Cdp.Browser.with_browser_exn
      ~f:(fun browser ->
        Cdp.Page.with_page_exn (Cdp.Browser.connection browser) ~f:(fun _page ->
          return ()))
      ()
  in
  [%expect
    {| 1970-01-01 00:00:00.000000Z ("Websocket closed"(reason Normal_closure)(msg"Pipe was closed")) |}];
  return ()
;;

let%expect_test "Browser.close cleans up the user-data-dir" =
  (* [Core_unix.mkdtemp] gives every profile a distinct ".tmp.<random>" suffix. Diff the
     {e real} directory names so a previous test's not-yet-cleaned profile (which would
     otherwise scrub to the same shape) cancels out; only then scrub for a stable display. *)
  let scrub_random_suffix s =
    match String.rsplit2 s ~on:'.' with
    | Some (prefix, _) -> prefix ^ ".<random>"
    | None -> s
  in
  let cdp_profiles () =
    let%map entries = Sys.ls_dir Filename.temp_dir_name in
    List.filter entries ~f:(String.is_prefix ~prefix:"cdp-profile-") |> String.Set.of_list
  in
  let scrub set =
    Set.to_list set |> List.map ~f:scrub_random_suffix |> String.Set.of_list
  in
  let%bind before = cdp_profiles () in
  let%bind () =
    Cdp.Browser.with_browser_exn
      ~f:(fun _ ->
        let%map during = cdp_profiles () in
        print_s [%sexp (scrub (Set.diff during before) : String.Set.t)];
        [%expect {| (cdp-profile-XXXXXX.tmp.<random>) |}])
      ()
  in
  let%bind after = cdp_profiles () in
  print_s [%sexp (scrub (Set.diff after before) : String.Set.t)];
  [%expect
    {|
    1970-01-01 00:00:00.000000Z ("Websocket closed"(reason Normal_closure)(msg"Pipe was closed"))
    ()
    |}];
  return ()
;;
