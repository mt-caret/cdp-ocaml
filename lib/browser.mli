(** Spawn a chromium process with CDP enabled and own its browser-level WebSocket
    connection.

    [launch] discovers a Chromium binary via {!Chrome_path}, launches it with
    [--remote-debugging-port=0] and a fresh user-data-dir, parses the [DevToolsActivePort]
    file, then opens the browser-level CDP WebSocket. [close] issues [Browser.close] over
    that connection for a clean shutdown, falling back to SIGTERM and SIGKILL on
    timeout. *)

open! Core
open! Async

type t

val launch
  :  ?chrome_path:string
  -> ?headless:bool (** default [true]; when [false], omits [--headless=new] *)
  -> ?extra_args:string list
  -> unit
  -> t Deferred.Or_error.t

val launch_exn
  :  ?chrome_path:string
  -> ?headless:bool
  -> ?extra_args:string list
  -> unit
  -> t Deferred.t

(** Browser-level CDP connection. Use this to open new pages via {!Page.create}, run
    [Target.*] commands, etc. Don't [Connection.close] it — {!close} handles that. *)
val connection : t -> Connection.t

(** Send [Browser.close] over CDP, then escalate to SIGTERM/SIGKILL if the process
    doesn't exit within [graceful_timeout]/[term_timeout]. Removes the user-data-dir. *)
val close
  :  ?graceful_timeout:Time_ns.Span.t (** default 5s *)
  -> ?term_timeout:Time_ns.Span.t (** default 5s *)
  -> t
  -> unit Deferred.t

(** [with_browser ~f ()] launches a browser, passes it to [f], and ensures it is closed
    when [f] completes (success or failure). *)
val with_browser
  :  ?chrome_path:string
  -> ?headless:bool
  -> ?extra_args:string list
  -> f:(t -> 'a Deferred.Or_error.t)
  -> unit
  -> 'a Or_error.t Deferred.t

val with_browser_exn
  :  ?chrome_path:string
  -> ?headless:bool
  -> ?extra_args:string list
  -> f:(t -> 'a Deferred.t)
  -> unit
  -> 'a Deferred.t
