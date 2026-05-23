(** A page (tab) attached to a browser-level CDP connection via a flat session.

    Beyond transport ([create]/[navigate]/[screenshot]/[close]), [Page] is the entry point
    to the high-level DOM API: typed JS evaluation ({!eval}), page metadata, and keyboard
    input. Element queries and actions live in {!Locator}, which takes a [Page.t]. *)

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

(** The CDP session id attached to this page; useful for {!Connection.events_for_session}
    or sending raw page-scoped calls without going through {!navigate}/etc. *)
val session_id : t -> string

(** The CDP target id underlying this page. *)
val target_id : t -> string

(** {1 JavaScript evaluation}

    Evaluate an expression and decode its result per a {!Js_type} witness: e.g.
    [eval page Int "1 + 1"], [eval page String "document.title"], or [eval page Json expr]
    for the raw value. Pass an IIFE for multi-statement logic:
    [(function(){ ...; return 42; })()]. *)

val eval : t -> 'a Js_type.t -> string -> 'a Deferred.Or_error.t
val eval_exn : t -> 'a Js_type.t -> string -> 'a Deferred.t

(** {1 Page metadata} *)

val title : t -> string Deferred.Or_error.t
val title_exn : t -> string Deferred.t
val url : t -> string Deferred.Or_error.t
val url_exn : t -> string Deferred.t

(** Serialized [document.documentElement.outerHTML]. *)
val content : t -> string Deferred.Or_error.t

val content_exn : t -> string Deferred.t

(** {1 Keyboard (dispatched to the focused element)} *)

val press : t -> ?modifiers:Key.Modifier.t -> Key.t -> unit Deferred.Or_error.t
val press_exn : t -> ?modifiers:Key.Modifier.t -> Key.t -> unit Deferred.t

(** Insert [text] as if typed (via [Input.insertText]), firing real input events. *)
val type_text : t -> string -> unit Deferred.Or_error.t

val type_text_exn : t -> string -> unit Deferred.t
