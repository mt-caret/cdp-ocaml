(** A page (tab) attached to a browser-level CDP connection via a flat session. *)

open! Core
open! Async

type t

(** Open a new about:blank tab on [connection] and attach a flat session. The connection
    must be a browser-level connection (the one returned by [Browser.connection]). *)
val create : Connection.t -> t Deferred.Or_error.t

val create_exn : Connection.t -> t Deferred.t

(** Send a page-scoped CDP method on this page's attached session. *)
val call : t -> ('p, 'r, [ `Page ]) Method.t -> 'p -> 'r Deferred.Or_error.t

(** Navigate the page and wait for [Page.loadEventFired]. [load_timeout] caps how long
    we wait for the page to fire its load event after the navigate command returns;
    defaults to 30 seconds. *)
val navigate : ?load_timeout:Time_ns.Span.t -> t -> url:string -> unit Deferred.Or_error.t

val navigate_exn : ?load_timeout:Time_ns.Span.t -> t -> url:string -> unit Deferred.t

(** Take a screenshot of the current viewport. Returns raw PNG bytes. *)
val screenshot_png : t -> string Deferred.Or_error.t

val screenshot_png_exn : t -> string Deferred.t
val close : t -> unit Deferred.Or_error.t
val close_exn : t -> unit Deferred.t

(** [with_page connection ~f] creates a new page, passes it to [f], and ensures it is
    closed when [f] completes. Close errors during cleanup are swallowed. *)
val with_page
  :  Connection.t
  -> f:(t -> 'a Deferred.Or_error.t)
  -> 'a Or_error.t Deferred.t

val with_page_exn : Connection.t -> f:(t -> 'a Deferred.t) -> 'a Deferred.t

(** The CDP session id attached to this page; useful for filtering {!Connection.events}
    or sending raw page-scoped calls without going through {!navigate}/etc. *)
val session_id : t -> string

(** The CDP target id underlying this page. *)
val target_id : t -> string
