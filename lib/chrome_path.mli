(** Locate a Chromium-compatible browser executable.

    Search order, first hit wins:
    + Env vars (in order): [CDP_CHROME_PATH], [PUPPETEER_EXECUTABLE_PATH], [CHROME].
    + Binary names on [PATH] (in order): [chrome-headless-shell], [google-chrome],
      [google-chrome-stable], [chromium], [chromium-browser], [chrome].
    + OS-specific well-known install paths (Linux apt/snap layouts, macOS [/Applications]).

    Does not download or install anything. *)

open! Core
open! Async

val find : unit -> string Deferred.Or_error.t
