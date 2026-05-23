(** Typed CDP method bindings, one module per [Domain.method].

    Each method has a [Method.t] value plus [Params] and/or [Result] submodules with the
    typed record shapes. The scope phantom in [Method.t] encodes which channel the method
    must be dispatched on:
    - [[ `Browser ]]: send via {!Connection.call} on the browser-level connection.
    - [[ `Page ]]: send via [Page.call] on a page handle (which carries the session_id).

    Methods that legitimately work in either scope would be declared with [_] scope. *)

open! Core

module Browser : sig
  module Close : sig
    val method_ : (unit, unit, [ `Browser ]) Method.t
  end
end

module Target : sig
  module Create_target : sig
    module Params : sig
      type t = { url : string } [@@deriving jsonaf_of]
    end

    module Result : sig
      type t = { target_id : string } [@@deriving of_jsonaf]
    end

    val method_ : (Params.t, Result.t, [ `Browser ]) Method.t
  end

  module Attach_to_target : sig
    module Params : sig
      type t =
        { target_id : string
        ; flatten : bool
        }
      [@@deriving jsonaf_of]
    end

    module Result : sig
      type t = { session_id : string } [@@deriving of_jsonaf]
    end

    val method_ : (Params.t, Result.t, [ `Browser ]) Method.t
  end

  module Close_target : sig
    module Params : sig
      type t = { target_id : string } [@@deriving jsonaf_of]
    end

    val method_ : (Params.t, unit, [ `Browser ]) Method.t
  end
end

module Page : sig
  module Enable : sig
    val method_ : (unit, unit, [ `Page ]) Method.t
  end

  module Navigate : sig
    module Params : sig
      type t = { url : string } [@@deriving jsonaf_of]
    end

    module Result : sig
      type t = { frame_id : string } [@@deriving of_jsonaf]
    end

    val method_ : (Params.t, Result.t, [ `Page ]) Method.t
  end

  module Capture_screenshot : sig
    module Result : sig
      type t = { data : string } [@@deriving of_jsonaf]
    end

    val method_ : (unit, Result.t, [ `Page ]) Method.t
  end

  (** The wire name of the [Page.loadEventFired] event. *)
  val load_event_fired_method_name : string
end

(** The CDP [RemoteObject.type]. *)
module Remote_object_type : sig
  type t =
    | Object
    | Function
    | Undefined
    | String
    | Number
    | Boolean
    | Symbol
    | Bigint
  [@@deriving enumerate, sexp_of, to_string]
end

(** A CDP RemoteObject decoded by its [type]: serializable JS types carry an OCaml value;
    [Undefined]/[Function]/[Symbol]/[Bigint] have no by-value JSON representation. With
    [returnByValue], CDP sends no [subtype], so Date/Map/Set/Error all collapse to [Object]
    of an empty JSON object. *)
module Remote_object : sig
  (** [of_jsonaf] decodes a raw JS-result JSON value structurally (it never yields
      [Undefined]/[Function]/[Symbol]/[Bigint], which JSON can't represent). *)
  type t =
    | Null
    | Undefined
    | Boolean of bool
    | Number of float
    | String of string
    | Object of Jsonaf.t
    | Function
    | Symbol
    | Bigint of Bigint.t
  [@@deriving sexp_of, of_jsonaf]
end

module Runtime : sig
  (** [Runtime.evaluate] — evaluate a JS expression in the page and return its value. *)
  module Evaluate : sig
    module Params : sig
      type t =
        { expression : string
        ; return_by_value : bool
        ; await_promise : bool
        }
      [@@deriving jsonaf_of]
    end

    module Result : sig
      (** The outcome of an evaluation: a returned RemoteObject, or a thrown exception
          carrying the bare [exceptionDetails.text] and the exception's richer
          [description] (the message-plus-stack) when present. *)
      type t =
        | Returned of Remote_object.t
        | Exception of
            { text : string
            ; description : string option
            }
      [@@deriving sexp_of]
    end

    val method_ : (Params.t, Result.t, [ `Page ]) Method.t
  end
end

module Input : sig
  (** [Input.dispatchKeyEvent] — synthesize a single key event. A "press" is a [keyDown]
      followed by a [keyUp] with the same fields. *)
  module Dispatch_key_event : sig
    (** The event kind. Serializes to the CDP wire strings ["keyDown"] / ["keyUp"] (used by
        {!Params}'s derived serializer). *)
    module Event_type : sig
      type t =
        | Key_down
        | Key_up
    end

    module Params : sig
      type t =
        { type_ : Event_type.t
        ; modifiers : int (** bitmask: Alt=1, Control=2, Meta=4, Shift=8 *)
        ; key : string
        ; code : string
        ; windows_virtual_key_code : int
        ; text : string option (** omitted from the wire when [None] *)
        }
      [@@deriving jsonaf_of]
    end

    val method_ : (Params.t, unit, [ `Page ]) Method.t
  end

  (** [Input.insertText] — insert text as if typed, firing real input events without
      per-key code mapping. *)
  module Insert_text : sig
    module Params : sig
      type t = { text : string } [@@deriving jsonaf_of]
    end

    val method_ : (Params.t, unit, [ `Page ]) Method.t
  end
end
