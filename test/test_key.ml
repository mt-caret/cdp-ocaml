open! Core
module Event_type = Cdp.Protocol.Input.Dispatch_key_event.Event_type
module Modifier = Cdp.Key.Modifier

(* Render the wire JSON [Input.dispatchKeyEvent] would carry for a key, so the expect
   block documents the exact key/code/vkey/text/modifier mapping. *)
let show ?(modifiers = Modifier.empty) ?(event_type = Event_type.Key_down) key =
  Cdp.Key.dispatch_params key ~modifiers ~event_type
  |> [%jsonaf_of: Cdp.Protocol.Input.Dispatch_key_event.Params.t]
  |> Jsonaf.to_string
  |> print_endline
;;

let%expect_test "every named key maps to key/code/vkey/text" =
  (* [Char] enumerates all 256 chars, so exclude it here (covered separately below). Any
     key added to [Key.t] is automatically exercised. *)
  List.filter [%all: Cdp.Key.t] ~f:(function
    | Char _ -> false
    | _ -> true)
  |> List.iter ~f:show;
  [%expect
    {|
    {"type":"keyDown","modifiers":0,"key":"Enter","code":"Enter","windowsVirtualKeyCode":13,"text":"\r"}
    {"type":"keyDown","modifiers":0,"key":"Tab","code":"Tab","windowsVirtualKeyCode":9}
    {"type":"keyDown","modifiers":0,"key":"Escape","code":"Escape","windowsVirtualKeyCode":27}
    {"type":"keyDown","modifiers":0,"key":"Backspace","code":"Backspace","windowsVirtualKeyCode":8}
    {"type":"keyDown","modifiers":0,"key":"Delete","code":"Delete","windowsVirtualKeyCode":46}
    {"type":"keyDown","modifiers":0,"key":" ","code":"Space","windowsVirtualKeyCode":32,"text":" "}
    {"type":"keyDown","modifiers":0,"key":"ArrowUp","code":"ArrowUp","windowsVirtualKeyCode":38}
    {"type":"keyDown","modifiers":0,"key":"ArrowDown","code":"ArrowDown","windowsVirtualKeyCode":40}
    {"type":"keyDown","modifiers":0,"key":"ArrowLeft","code":"ArrowLeft","windowsVirtualKeyCode":37}
    {"type":"keyDown","modifiers":0,"key":"ArrowRight","code":"ArrowRight","windowsVirtualKeyCode":39}
    {"type":"keyDown","modifiers":0,"key":"Home","code":"Home","windowsVirtualKeyCode":36}
    {"type":"keyDown","modifiers":0,"key":"End","code":"End","windowsVirtualKeyCode":35}
    {"type":"keyDown","modifiers":0,"key":"PageUp","code":"PageUp","windowsVirtualKeyCode":33}
    {"type":"keyDown","modifiers":0,"key":"PageDown","code":"PageDown","windowsVirtualKeyCode":34}
    {"type":"keyDown","modifiers":0,"key":"Insert","code":"Insert","windowsVirtualKeyCode":45}
    {"type":"keyDown","modifiers":0,"key":"F1","code":"F1","windowsVirtualKeyCode":112}
    {"type":"keyDown","modifiers":0,"key":"F2","code":"F2","windowsVirtualKeyCode":113}
    {"type":"keyDown","modifiers":0,"key":"F3","code":"F3","windowsVirtualKeyCode":114}
    {"type":"keyDown","modifiers":0,"key":"F4","code":"F4","windowsVirtualKeyCode":115}
    {"type":"keyDown","modifiers":0,"key":"F5","code":"F5","windowsVirtualKeyCode":116}
    {"type":"keyDown","modifiers":0,"key":"F6","code":"F6","windowsVirtualKeyCode":117}
    {"type":"keyDown","modifiers":0,"key":"F7","code":"F7","windowsVirtualKeyCode":118}
    {"type":"keyDown","modifiers":0,"key":"F8","code":"F8","windowsVirtualKeyCode":119}
    {"type":"keyDown","modifiers":0,"key":"F9","code":"F9","windowsVirtualKeyCode":120}
    {"type":"keyDown","modifiers":0,"key":"F10","code":"F10","windowsVirtualKeyCode":121}
    {"type":"keyDown","modifiers":0,"key":"F11","code":"F11","windowsVirtualKeyCode":122}
    {"type":"keyDown","modifiers":0,"key":"F12","code":"F12","windowsVirtualKeyCode":123}
    |}]
;;

let%expect_test "character keys map to key/code/vkey/text" =
  show (Char 'k');
  show (Char 'a');
  show (Char '5');
  [%expect
    {|
    {"type":"keyDown","modifiers":0,"key":"k","code":"KeyK","windowsVirtualKeyCode":75,"text":"k"}
    {"type":"keyDown","modifiers":0,"key":"a","code":"KeyA","windowsVirtualKeyCode":65,"text":"a"}
    {"type":"keyDown","modifiers":0,"key":"5","code":"Digit5","windowsVirtualKeyCode":53,"text":"5"}
    |}]
;;

let%expect_test "a non-Shift modifier drops [text]; Shift keeps it" =
  show ~modifiers:Modifier.control (Char 'k');
  show ~modifiers:Modifier.shift (Char 'a');
  show ~modifiers:Modifier.(control + shift) (Char 'k');
  show ~modifiers:Modifier.meta (Char 'c');
  [%expect
    {|
    {"type":"keyDown","modifiers":2,"key":"k","code":"KeyK","windowsVirtualKeyCode":75}
    {"type":"keyDown","modifiers":8,"key":"a","code":"KeyA","windowsVirtualKeyCode":65,"text":"a"}
    {"type":"keyDown","modifiers":10,"key":"k","code":"KeyK","windowsVirtualKeyCode":75}
    {"type":"keyDown","modifiers":4,"key":"c","code":"KeyC","windowsVirtualKeyCode":67}
    |}]
;;

let%expect_test "keyUp uses the same fields with type:keyUp" =
  show ~event_type:Event_type.Key_up (Char 'k');
  show ~event_type:Event_type.Key_up Enter;
  [%expect
    {|
    {"type":"keyUp","modifiers":0,"key":"k","code":"KeyK","windowsVirtualKeyCode":75,"text":"k"}
    {"type":"keyUp","modifiers":0,"key":"Enter","code":"Enter","windowsVirtualKeyCode":13,"text":"\r"}
    |}]
;;

let%expect_test "modifier flags combine into the CDP bitmask" =
  show ~modifiers:Modifier.alt Escape;
  show ~modifiers:Modifier.(control + alt + meta + shift) Escape;
  [%expect
    {|
    {"type":"keyDown","modifiers":1,"key":"Escape","code":"Escape","windowsVirtualKeyCode":27}
    {"type":"keyDown","modifiers":15,"key":"Escape","code":"Escape","windowsVirtualKeyCode":27}
    |}]
;;
