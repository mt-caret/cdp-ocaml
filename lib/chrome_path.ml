open! Core
open! Async

let env_var_candidates = [ "CDP_CHROME_PATH"; "PUPPETEER_EXECUTABLE_PATH"; "CHROME" ]

let binary_names =
  [ "chrome-headless-shell"
  ; "google-chrome"
  ; "google-chrome-stable"
  ; "chromium"
  ; "chromium-browser"
  ; "chrome"
  ]
;;

let well_known_paths =
  [ (* Linux *)
    "/usr/bin/google-chrome"
  ; "/usr/bin/google-chrome-stable"
  ; "/usr/bin/chromium"
  ; "/usr/bin/chromium-browser"
  ; "/snap/bin/chromium"
  ; (* macOS *)
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
  ; "/Applications/Chromium.app/Contents/MacOS/Chromium"
  ; "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge"
  ]
;;

let exists path =
  match%map Sys.file_exists path with
  | `Yes -> true
  | `No | `Unknown -> false
;;

let which name =
  let path_dirs = Sys.getenv "PATH" |> Option.value ~default:"" |> String.split ~on:':' in
  Deferred.List.find_map path_dirs ~f:(fun dir ->
    let candidate = dir ^/ name in
    let%map exists = exists candidate in
    match exists with
    | true -> Some candidate
    | false -> None)
;;

let try_env () =
  match List.find_map env_var_candidates ~f:Sys.getenv with
  | None -> return None
  | Some path ->
    let%map exists = exists path in
    (match exists with
     | true -> Some path
     | false -> None)
;;

let try_path () = Deferred.List.find_map binary_names ~f:which
let try_well_known () = Deferred.List.find well_known_paths ~f:exists

(** Probe the Playwright cache (~/.cache/ms-playwright on Linux,
    ~/Library/Caches/ms-playwright on macOS) for a Chromium install, preferring the
    highest-numbered build. *)
let try_playwright_cache () =
  let cache_roots =
    let home = Sys.getenv "HOME" |> Option.value ~default:"" in
    [ home ^/ ".cache" ^/ "ms-playwright"
    ; home ^/ "Library" ^/ "Caches" ^/ "ms-playwright"
    ]
  in
  let binary_subpaths =
    [ "chrome-headless-shell-linux64" ^/ "chrome-headless-shell"
    ; "chrome-headless-shell-mac" ^/ "chrome-headless-shell"
    ; "chrome-linux" ^/ "chrome"
    ; "chrome-linux" ^/ "headless_shell"
    ; "chrome-mac" ^/ "Chromium.app" ^/ "Contents" ^/ "MacOS" ^/ "Chromium"
    ]
  in
  Deferred.List.find_map cache_roots ~f:(fun root ->
    match%bind exists root with
    | false -> return None
    | true ->
      let%bind entries =
        match%map Monitor.try_with ~rest:`Log (fun () -> Sys.readdir root) with
        | Ok arr -> Array.to_list arr
        | Error _ -> []
      in
      let chromium_dirs =
        entries
        |> List.filter ~f:(String.is_prefix ~prefix:"chromium")
        |> List.sort ~compare:(Comparable.reverse String.compare)
      in
      Deferred.List.find_map chromium_dirs ~f:(fun dir ->
        let base = root ^/ dir in
        Deferred.List.find_map binary_subpaths ~f:(fun sub ->
          let candidate = base ^/ sub in
          let%map exists = exists candidate in
          match exists with
          | true -> Some candidate
          | false -> None)))
;;

let find () =
  let attempts = [ try_env; try_path; try_well_known; try_playwright_cache ] in
  let%map result = Deferred.List.find_map attempts ~f:(fun attempt -> attempt ()) in
  match result with
  | Some path -> Ok path
  | None ->
    Or_error.error_s
      [%message
        "could not locate a Chrome/Chromium browser; set CDP_CHROME_PATH to override"
          ~searched_env_vars:(env_var_candidates : string list)
          ~searched_binary_names:(binary_names : string list)
          ~searched_paths:(well_known_paths : string list)]
;;
