(** Pure, composable descriptions of element selection.

    A {!t} is a single selection {e step}; a selection is a [t Nonempty_list.t] of steps
    applied in order (start from [document], then each step refines the running set). A
    locator carries no connection — it's plain data you can build, [sexp_of], and
    {!to_js} without touching the browser. Actions and queries take a {!Page.t} and the
    step list; thanks to type-directed disambiguation the list can be written as a bare
    literal, e.g. [Locator.click_exn page [ Locator.css "#save" ]].

    Element actions/queries auto-wait: they poll for a match for a bounded window (~5s)
    before acting, and never wait unbounded. [count] and [is_visible] report the current
    state without waiting. *)

open! Core
open! Async

type t [@@deriving sexp_of]

(** {1 Steps (pure constructors)} *)

(** Match by CSS selector. *)
val css : string -> t

(** Match the innermost elements whose [textContent] contains the string. *)
val text : string -> t

(** Match by placeholder attribute (sugar for a [\[placeholder="…"\]] CSS step). *)
val placeholder : string -> t

(** Keep only elements whose [textContent] contains the string (a filter step). *)
val has_text : string -> t

(** Narrow to the element at index [i] (0-based). *)
val nth : int -> t

(** [first = nth 0]. *)
val first : t

(** Narrow to the last match. *)
val last : t

(** The JS expression (an IIFE) that evaluates to the array of matched elements. Exposed
    for inspection/testing; the action functions build on it. *)
val to_js : t Nonempty_list.t -> string

(** {1 Queries} *)

(** Number of matches right now (no auto-wait). *)
val count : Page.t -> t Nonempty_list.t -> int Deferred.Or_error.t

val count_exn : Page.t -> t Nonempty_list.t -> int Deferred.t

(** Whether the first match is currently visible (no auto-wait; [false] if none match). *)
val is_visible : Page.t -> t Nonempty_list.t -> bool Deferred.Or_error.t

val is_visible_exn : Page.t -> t Nonempty_list.t -> bool Deferred.t
val inner_text : Page.t -> t Nonempty_list.t -> string Deferred.Or_error.t
val inner_text_exn : Page.t -> t Nonempty_list.t -> string Deferred.t

(** [textContent] of the first match; [None] if the property is JS [null]. *)
val text_content : Page.t -> t Nonempty_list.t -> string option Deferred.Or_error.t

val text_content_exn : Page.t -> t Nonempty_list.t -> string option Deferred.t
val input_value : Page.t -> t Nonempty_list.t -> string Deferred.Or_error.t
val input_value_exn : Page.t -> t Nonempty_list.t -> string Deferred.t

(** [getAttribute name] of the first match; [None] when the attribute is absent. *)
val get_attribute
  :  Page.t
  -> t Nonempty_list.t
  -> string
  -> string option Deferred.Or_error.t

val get_attribute_exn : Page.t -> t Nonempty_list.t -> string -> string option Deferred.t

(** {1 Actions} *)

val click : Page.t -> t Nonempty_list.t -> unit Deferred.Or_error.t
val click_exn : Page.t -> t Nonempty_list.t -> unit Deferred.t

(** Focus the first match, set its [value], and fire bubbling [input]/[change] events —
    enough for virtual-dom frameworks that re-read [target.value] on [input]. *)
val fill : Page.t -> t Nonempty_list.t -> text:string -> unit Deferred.Or_error.t

val fill_exn : Page.t -> t Nonempty_list.t -> text:string -> unit Deferred.t
val focus : Page.t -> t Nonempty_list.t -> unit Deferred.Or_error.t
val focus_exn : Page.t -> t Nonempty_list.t -> unit Deferred.t
val blur : Page.t -> t Nonempty_list.t -> unit Deferred.Or_error.t
val blur_exn : Page.t -> t Nonempty_list.t -> unit Deferred.t
val hover : Page.t -> t Nonempty_list.t -> unit Deferred.Or_error.t
val hover_exn : Page.t -> t Nonempty_list.t -> unit Deferred.t

(** Focus the first match, then press [key] with [modifiers] held. *)
val press
  :  Page.t
  -> t Nonempty_list.t
  -> ?modifiers:Key.Modifier.t
  -> Key.t
  -> unit Deferred.Or_error.t

val press_exn
  :  Page.t
  -> t Nonempty_list.t
  -> ?modifiers:Key.Modifier.t
  -> Key.t
  -> unit Deferred.t

(** Focus the first match, then type [text] via [Input.insertText]. *)
val type_text : Page.t -> t Nonempty_list.t -> string -> unit Deferred.Or_error.t

val type_text_exn : Page.t -> t Nonempty_list.t -> string -> unit Deferred.t

(** {1 Waiting} *)

(** Poll (bounded) until the locator reaches the requested state. *)
val wait_for
  :  Page.t
  -> t Nonempty_list.t
  -> [ `Attached | `Detached | `Visible | `Hidden ]
  -> unit Deferred.Or_error.t

val wait_for_exn
  :  Page.t
  -> t Nonempty_list.t
  -> [ `Attached | `Detached | `Visible | `Hidden ]
  -> unit Deferred.t
