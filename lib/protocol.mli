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
