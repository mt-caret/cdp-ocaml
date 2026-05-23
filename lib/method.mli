(** Typed description of a CDP method.

    Each [('p, 'r, 'scope) t] knows its CDP method name plus the conversions between
    OCaml types [('p, 'r)] and the wire-format JSON. The phantom [_scope] parameter
    encodes whether the method is dispatched on a browser-level connection
    ([[ `Browser ]]) or on an attached page session ([[ `Page ]]). *)

open! Core

type ('p, 'r, 'scope) t = private
  { name : string
  ; params_to_jsonaf : 'p -> Jsonaf.t
  ; result_of_jsonaf : Jsonaf.t -> 'r
  }

val create
  :  name:string
  -> params_to_jsonaf:('p -> Jsonaf.t)
  -> result_of_jsonaf:(Jsonaf.t -> 'r)
  -> ('p, 'r, 'scope) t

(** Serialize [unit] as the JSON empty object [{}] — the conventional wire shape for CDP
    methods that take no parameters. *)
val empty_params : unit -> Jsonaf.t

(** Drop any result body — for CDP methods whose response is uninteresting. *)
val ignore_result : Jsonaf.t -> unit
