open! Core

module Modifier = struct
  (* CDP modifier bitmask bits: Alt=1, Control=2, Meta=4, Shift=8. *)
  let alt = Flags.create ~bit:0
  let control = Flags.create ~bit:1
  let meta = Flags.create ~bit:2
  let shift = Flags.create ~bit:3

  include Flags.Make (struct
      let known = [ shift, "shift"; meta, "meta"; control, "control"; alt, "alt" ]
      let remove_zero_flags = false
      let allow_intersecting = false
      let should_print_error = true
    end)
end

type t =
  | Char of Char.t [@stringable.nested ""]
  | Enter
  | Tab
  | Escape
  | Backspace
  | Delete
  | Space
  | Arrow_up
  | Arrow_down
  | Arrow_left
  | Arrow_right
  | Home
  | End
  | Page_up
  | Page_down
  | Insert
  | F1
  | F2
  | F3
  | F4
  | F5
  | F6
  | F7
  | F8
  | F9
  | F10
  | F11
  | F12
[@@deriving enumerate, sexp_of, to_string ~capitalize:"PascalCase"]

let dispatch_params t ~modifiers ~event_type =
  (* [to_string] (PascalCase) gives the CDP [code] for every named key — and the [key]
     too, except for [Space] — and for [Char c] it gives the character itself. *)
  let name = to_string t in
  let key, code, natural_text =
    match t with
    | Char c ->
      let code =
        match Char.is_alpha c, Char.is_digit c with
        | true, _ -> "Key" ^ String.uppercase name
        | _, true -> "Digit" ^ name
        | false, false -> ""
      in
      name, code, Some name
    | Space -> " ", name, Some " "
    | Enter -> name, name, Some "\r"
    | Tab
    | Escape
    | Backspace
    | Delete
    | Arrow_up
    | Arrow_down
    | Arrow_left
    | Arrow_right
    | Home
    | End
    | Page_up
    | Page_down
    | Insert
    | F1
    | F2
    | F3
    | F4
    | F5
    | F6
    | F7
    | F8
    | F9
    | F10
    | F11
    | F12 -> name, name, None
  in
  let windows_virtual_key_code =
    match t with
    | Char c ->
      (match Char.is_alpha c with
       | true -> Char.to_int (Char.uppercase c)
       | false -> Char.to_int c)
    | Enter -> 13
    | Tab -> 9
    | Escape -> 27
    | Backspace -> 8
    | Delete -> 46
    | Space -> 32
    | Arrow_up -> 38
    | Arrow_down -> 40
    | Arrow_left -> 37
    | Arrow_right -> 39
    | Home -> 36
    | End -> 35
    | Page_up -> 33
    | Page_down -> 34
    | Insert -> 45
    | F1 -> 112
    | F2 -> 113
    | F3 -> 114
    | F4 -> 115
    | F5 -> 116
    | F6 -> 117
    | F7 -> 118
    | F8 -> 119
    | F9 -> 120
    | F10 -> 121
    | F11 -> 122
    | F12 -> 123
  in
  (* Chromium suppresses [text] when a non-Shift modifier is held (e.g. Control+k must not
     insert a "k"). *)
  let text =
    match Modifier.do_intersect modifiers Modifier.(control + alt + meta) with
    | true -> None
    | false -> natural_text
  in
  { Protocol.Input.Dispatch_key_event.Params.type_ = event_type
  ; modifiers = Modifier.to_int_exn modifiers
  ; key
  ; code
  ; windows_virtual_key_code
  ; text
  }
;;
