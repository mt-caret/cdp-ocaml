(** Witnesses for the OCaml type expected back from a JavaScript evaluation.

    Passing a [_ t] to {!Page.eval} both fixes and decodes the result type, so a single
    [eval] subsumes a family of [eval_string]/[eval_int]/… functions. *)

open! Core

type _ t =
  | String : string t
  | Int : int t (** an integral [Number] *)
  | Bool : bool t
  | String_opt : string option t (** a string, or JS [null] as [None] *)
  | Object : Jsonaf.t t (** the raw JSON of a JS object or array *)
  | Undefined : unit t

(** Decode a {!Protocol.Remote_object.t} according to the witness. *)
val decode : 'a t -> Protocol.Remote_object.t -> 'a Or_error.t
