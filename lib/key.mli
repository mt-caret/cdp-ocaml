(** Keyboard keys and modifiers, plus their mapping to the [Input.dispatchKeyEvent] wire
    fields. *)

open! Core

module Modifier : sig
  type t [@@deriving sexp_of]

  val empty : t
  val alt : t
  val control : t
  val meta : t
  val shift : t

  (** Set union. *)
  val ( + ) : t -> t -> t
end

type t =
  | Char of char
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
[@@deriving enumerate, sexp_of]

(** The [Input.dispatchKeyEvent] params for one key event. [text] (the character the key
    produces) is included only when the key has text and no non-Shift modifier is held —
    matching Chromium's behavior, where e.g. Control+k must not insert a "k". *)
val dispatch_params
  :  t
  -> modifiers:Modifier.t
  -> event_type:Protocol.Input.Dispatch_key_event.Event_type.t
  -> Protocol.Input.Dispatch_key_event.Params.t
