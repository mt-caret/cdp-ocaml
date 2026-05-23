open! Core
open! Async
open Test_helpers
module Page = Cdp.Page
module Locator = Cdp.Locator
module Key = Cdp.Key
module Modifier = Cdp.Key.Modifier
module Js_type = Cdp.Js_type

(* The chosen port is random; scrub it so the navigated URL is stable across runs. *)
let scrub_port url =
  match String.rsplit2 url ~on:':' with
  | None -> url
  | Some (prefix, rest) ->
    (match String.lsplit2 rest ~on:'/' with
     | None -> url
     | Some (_port, path) -> [%string "%{prefix}:<port>/%{path}"])
;;

let%expect_test "Locator.to_js compiles a step chain to JS (pure; no browser)" =
  let show chain = print_endline (Locator.to_js chain) in
  show [ Locator.css "div" ];
  show [ Locator.css "#list button"; Locator.first ];
  show [ Locator.css "button"; Locator.has_text "Save"; Locator.last ];
  show [ Locator.placeholder "Email" ];
  [%expect
    {|
    (function(){
      let els = [document];
      els = els.flatMap(r => Array.from(r.querySelectorAll("div")));
      return els;
    })()
    (function(){
      let els = [document];
      els = els.flatMap(r => Array.from(r.querySelectorAll("#list button")));
      els = (els[0] !== undefined) ? [els[0]] : [];
      return els;
    })()
    (function(){
      let els = [document];
      els = els.flatMap(r => Array.from(r.querySelectorAll("button")));
      els = els.filter(e => e.textContent.includes("Save"));
      els = els.length ? [els[els.length - 1]] : [];
      return els;
    })()
    (function(){
      let els = [document];
      els = els.flatMap(r => Array.from(r.querySelectorAll("[placeholder=\"Email\"]")));
      return els;
    })()
    |}];
  return ()
;;

let%expect_test "Page.content / title / url" =
  let%bind () =
    with_page_on_html
      ~html:
        "<html><head><title>Fixture Title</title></head><body><div \
         data-marker=\"cdp-content-probe\">Hello world</div></body></html>"
      ~f:(fun page ->
        let%bind title = Page.title_exn page in
        let%bind url = Page.url_exn page in
        let%bind content = Page.content_exn page in
        (* Full HTML varies by Chrome build, so just check for a known marker substring. *)
        let has_marker =
          String.is_substring content ~substring:"data-marker=\"cdp-content-probe\""
        in
        print_s
          [%message
            "" (title : string) ~url:(scrub_port url : string) (has_marker : bool)];
        [%expect
          {| ((title "Fixture Title") (url http://127.0.0.1:<port>/) (has_marker true)) |}];
        return ())
  in
  [%expect
    {| 1970-01-01 00:00:00.000000Z ("Websocket closed"(reason Normal_closure)(msg"Pipe was closed")) |}];
  return ()
;;

let fill_fixture =
  {|<html><body>
    <input id="inp" type="text" value="initial" data-test="field" />
    <script>
      window.__inputs = [];
      document.getElementById('inp').addEventListener('input', function (e) {
        window.__inputs.push(e.target.value);
      });
    </script>
  </body></html>|}
;;

let%expect_test
    "Locator.fill fires input events (virtual-dom compatible) and updates value"
  =
  let%bind () =
    with_page_on_html ~html:fill_fixture ~f:(fun page ->
      let inp = Locator.css "#inp" in
      let%bind () = Locator.fill_exn page [ inp ] ~text:"hello" in
      let%bind value = Locator.input_value_exn page [ inp ] in
      (* the [input] handler recorded the new value, proving a real input event fired *)
      let%bind input_events =
        Page.eval_exn page Js_type.String "window.__inputs.join('|')"
      in
      let%bind present = Locator.get_attribute_exn page [ inp ] "data-test" in
      let%bind absent = Locator.get_attribute_exn page [ inp ] "data-missing" in
      print_s
        [%message
          ""
            (value : string)
            (input_events : string)
            (present : string option)
            (absent : string option)];
      [%expect {| ((value hello) (input_events hello) (present (field)) (absent ())) |}];
      return ())
  in
  [%expect
    {| 1970-01-01 00:00:00.000000Z ("Websocket closed"(reason Normal_closure)(msg"Pipe was closed")) |}];
  return ()
;;

let locator_fixture =
  {|<html><body>
    <div id="list">
      <button>Save</button>
      <button>Cancel</button>
      <button>Delete</button>
    </div>
    <div id="vis">visible</div>
    <div id="hid" style="display:none">hidden</div>
    <button id="toggle" onclick="window.__clicks = (window.__clicks || 0) + 1; document.getElementById('panel').textContent = 'clicked ' + window.__clicks">Toggle</button>
    <div id="panel">closed</div>
    <div id="hov" onmouseover="window.__hovered = 'hov'">hovertarget</div>
  </body></html>|}
;;

let%expect_test
    "Locator queries: count / first / last / nth / has_text / inner_text / is_visible / \
     text_content"
  =
  let%bind () =
    with_page_on_html ~html:locator_fixture ~f:(fun page ->
      let buttons = Locator.css "#list button" in
      let%bind count = Locator.count_exn page [ buttons ] in
      let%bind first = Locator.inner_text_exn page [ buttons; Locator.first ] in
      let%bind last = Locator.inner_text_exn page [ buttons; Locator.last ] in
      let%bind second = Locator.inner_text_exn page [ buttons; Locator.nth 1 ] in
      let%bind save_count = Locator.count_exn page [ buttons; Locator.has_text "Save" ] in
      let%bind save_text =
        Locator.inner_text_exn page [ buttons; Locator.has_text "Save" ]
      in
      print_s
        [%message
          "buttons"
            (count : int)
            (first : string)
            (last : string)
            (second : string)
            (save_count : int)
            (save_text : string)];
      [%expect
        {|
        (buttons (count 3) (first Save) (last Delete) (second Cancel) (save_count 1)
         (save_text Save))
        |}];
      let%bind vis = Locator.is_visible_exn page [ Locator.css "#vis" ] in
      let%bind hid = Locator.is_visible_exn page [ Locator.css "#hid" ] in
      let%bind hidden_text = Locator.text_content_exn page [ Locator.css "#hid" ] in
      print_s
        [%message "visibility" (vis : bool) (hid : bool) (hidden_text : string option)];
      [%expect {| (visibility (vis true) (hid false) (hidden_text (hidden))) |}];
      let%bind () = Locator.click_exn page [ Locator.text "Toggle" ] in
      let%bind after_get_by_text = Locator.inner_text_exn page [ Locator.css "#panel" ] in
      let%bind () = Locator.click_exn page [ Locator.css "#toggle" ] in
      let%bind after_css_click = Locator.inner_text_exn page [ Locator.css "#panel" ] in
      print_s [%message "toggle" (after_get_by_text : string) (after_css_click : string)];
      [%expect
        {| (toggle (after_get_by_text "clicked 1") (after_css_click "clicked 2")) |}];
      let%bind () = Locator.hover_exn page [ Locator.css "#hov" ] in
      let%bind hovered = Page.eval_exn page Js_type.String "window.__hovered || 'none'" in
      print_s [%message "hover" (hovered : string)];
      [%expect {| (hover (hovered hov)) |}];
      return ())
  in
  [%expect
    {| 1970-01-01 00:00:00.000000Z ("Websocket closed"(reason Normal_closure)(msg"Pipe was closed")) |}];
  return ()
;;

let keyboard_fixture =
  {|<html><body>
    <input id="k" />
    <script>
      window.__keys = [];
      window.addEventListener('keydown', function (e) {
        var mods =
          (e.ctrlKey ? 'C' : '') +
          (e.shiftKey ? 'S' : '') +
          (e.altKey ? 'A' : '') +
          (e.metaKey ? 'M' : '');
        window.__keys.push(mods + ':' + e.key);
      });
    </script>
  </body></html>|}
;;

let%expect_test
    "Page.press records modifiers/keys; type_text inserts; Locator.press focuses first"
  =
  let%bind () =
    with_page_on_html ~html:keyboard_fixture ~f:(fun page ->
      let input = Locator.css "#k" in
      let%bind () = Locator.focus_exn page [ input ] in
      let%bind () = Page.press_exn page ~modifiers:Modifier.control (Char 'k') in
      let%bind () = Page.press_exn page Escape in
      let%bind () = Page.press_exn page Arrow_down in
      (* insertText does not fire keydown, so it leaves __keys untouched *)
      let%bind () = Page.type_text_exn page "abc" in
      (* Locator.press focuses the element first, then presses (Enter adds no text) *)
      let%bind () = Locator.press_exn page [ input ] Enter in
      let%bind keys = Page.eval_exn page Js_type.String "window.__keys.join('|')" in
      let%bind typed = Locator.input_value_exn page [ input ] in
      print_s [%message "" (keys : string) (typed : string)];
      [%expect {| ((keys C:k|:Escape|:ArrowDown|:Enter) (typed abc)) |}];
      return ())
  in
  [%expect
    {| 1970-01-01 00:00:00.000000Z ("Websocket closed"(reason Normal_closure)(msg"Pipe was closed")) |}];
  return ()
;;

let wait_for_fixture =
  {|<html><body>
    <div id="container"></div>
    <script>
      setTimeout(function () {
        var d = document.createElement('div');
        d.id = 'late';
        d.textContent = 'arrived';
        document.getElementById('container').appendChild(d);
      }, 250);
    </script>
  </body></html>|}
;;

let%expect_test "actions auto-wait for a not-yet-rendered element; wait_for `Attached" =
  let%bind () =
    with_page_on_html ~html:wait_for_fixture ~f:(fun page ->
      let late = Locator.css "#late" in
      let%bind before = Locator.count_exn page [ late ] in
      (* inner_text polls until the element the script appends after 250ms shows up *)
      let%bind text = Locator.inner_text_exn page [ late ] in
      let%bind () = Locator.wait_for_exn page [ late ] `Attached in
      let%bind after = Locator.count_exn page [ late ] in
      print_s [%message "" (before : int) (text : string) (after : int)];
      [%expect {| ((before 0) (text arrived) (after 1)) |}];
      return ())
  in
  [%expect
    {| 1970-01-01 00:00:00.000000Z ("Websocket closed"(reason Normal_closure)(msg"Pipe was closed")) |}];
  return ()
;;
