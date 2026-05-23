(** A multiplexed CDP connection over a single WebSocket.

    Each call gets an auto-incrementing id; responses are routed back to the caller by id
    via an Ivar table. Events (messages with no [id]) are parsed into {!Event.t} and
    forwarded to the [events] pipe so callers can subscribe to [Page.loadEventFired]
    etc. *)

open! Core
open! Async

type t

module Event : sig
  (** A CDP event pushed from the browser. The session it belongs to is implied by which
      subscription it came from ({!events_for_session} vs {!events_without_session}). *)
  type t =
    { method_name : string
    ; params : Jsonaf.t option
    }
end

val connect : Uri.t -> t Deferred.Or_error.t
val connect_exn : Uri.t -> t Deferred.t

(** Send a browser-scoped CDP method on the top-level connection. To dispatch a
    page-scoped method, use {!Page.call} on a page handle instead — that route carries
    the session_id automatically. *)
val call : t -> ('p, 'r, [ `Browser ]) Method.t -> 'p -> 'r Deferred.Or_error.t

val call_exn : t -> ('p, 'r, [ `Browser ]) Method.t -> 'p -> 'r Deferred.t

(** The page-scope equivalent of {!call}. Page handles wrap this so callers don't pass
    [session_id] themselves; exposed here for sibling modules that maintain their own
    session id. *)
val call_page
  :  t
  -> session_id:string
  -> ('p, 'r, [ `Page ]) Method.t
  -> 'p
  -> 'r Deferred.Or_error.t

val call_page_exn
  :  t
  -> session_id:string
  -> ('p, 'r, [ `Page ]) Method.t
  -> 'p
  -> 'r Deferred.t

(** Escape hatch for methods not yet defined in {!Protocol}. Returns the raw [result]
    JSON object (or [`Object []] if the response has no [result] field). Accepts an
    optional [session_id] for page-scoped raw calls. *)
val call_raw
  :  t
  -> ?session_id:string
  -> method_:string
  -> params:Jsonaf.t
  -> unit
  -> Jsonaf.t Deferred.Or_error.t

val call_raw_exn
  :  t
  -> ?session_id:string
  -> method_:string
  -> params:Jsonaf.t
  -> unit
  -> Jsonaf.t Deferred.t

(** Subscribe to events for a particular attached session. Each call mints a fresh pipe
    and every matching event is broadcast to all live pipes, so independent consumers
    never compete for the same event. Close the reader to unsubscribe. *)
val events_for_session : t -> session_id:string -> Event.t Pipe.Reader.t

(** Like {!events_for_session} but for events carrying no session id (browser-level
    events such as [Target.attachedToTarget]). *)
val events_without_session : t -> Event.t Pipe.Reader.t

val close : t -> unit Deferred.t
