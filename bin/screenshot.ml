open! Core
open! Async

let take_screenshot ~url ~output_path ~chrome_path =
  Cdp.Browser.with_browser
    ?chrome_path
    ~f:(fun browser ->
      Cdp.Browser.connection browser
      |> Cdp.Page.with_page ~f:(fun page ->
        let open Deferred.Or_error.Let_syntax in
        let%bind () = Cdp.Page.navigate page ~url in
        let%bind png = Cdp.Page.screenshot_png page in
        Writer.save output_path ~contents:png |> Deferred.ok))
    ()
;;

let command =
  Command.async_or_error
    ~summary:"Open a URL in headless chromium via CDP and save a PNG screenshot."
    [%map_open.Command
      let url = anon ("URL" %: string)
      and output_path =
        flag
          "-o"
          (optional_with_default "screenshot.png" string)
          ~doc:"PATH output PNG path (default: screenshot.png)"
      and chrome_path =
        flag
          "-chrome"
          (optional string)
          ~doc:"PATH chrome binary (default: env/PATH/well-known discovery)"
      and () = Log.Global.set_level_via_param () in
      fun () -> take_screenshot ~url ~output_path ~chrome_path]
;;

let () = Command_unix.run command
