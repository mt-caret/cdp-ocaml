open! Core
open! Async
open Test_helpers
module Page = Cdp.Page
module Js_type = Cdp.Js_type
module Evaluate = Cdp.Protocol.Runtime.Evaluate
module Remote_object_type = Cdp.Protocol.Remote_object_type

(* JS exception stack traces carry run/version-dependent [line:col]; mask them so the
   output stays stable. *)
let mask_line_col =
  Re.Pcre.substitute ~rex:(Re.Pcre.regexp ":[0-9]+:[0-9]+") ~subst:(fun _ -> ":LINE:COL")
;;

let print_sexp_masked sexp = Sexp.to_string_hum sexp |> mask_line_col |> print_endline

(* The wire JSON carries per-run ids (32-hex session/target ids, dotted object ids) and
   stack [line:col]; scrub them to stable placeholders. Matching by value shape sidesteps
   the JSON's escaped quotes in the rendered log line; the [line:col] mask is anchored to
   [<anonymous>] so it leaves the log's own timestamp alone. *)
let mask_wire_log =
  let scrub pattern replacement =
    Re.Pcre.substitute ~rex:(Re.Pcre.regexp pattern) ~subst:(fun _ -> replacement)
  in
  fun s ->
    scrub "[A-F0-9]{32}" "<id>" s
    |> scrub {|-?[0-9]+\.[0-9]+\.[0-9]+|} "<object-id>"
    |> scrub "<anonymous>:[0-9]+:[0-9]+" "<anonymous>:LINE:COL"
;;

(* The witness fixes how the result is decoded and sexped, so one helper covers every
   result type. *)
let%expect_test "evaluation results and how they parse (real browser)" =
  let%bind () =
    with_page_on_html ~html:"<html><body></body></html>" ~f:(fun page ->
      let eval_and_print (type a) (witness : a Js_type.t) expr =
        let%map result = Page.eval page witness expr in
        let sexp_of_a : a -> Sexp.t =
          match witness with
          | String -> [%sexp_of: string]
          | Int -> [%sexp_of: int]
          | Bool -> [%sexp_of: bool]
          | String_opt -> [%sexp_of: string option]
          | Object -> [%sexp_of: Jsonaf.t]
          | Undefined -> [%sexp_of: unit]
        in
        print_sexp_masked ([%sexp_of: a Or_error.t] result)
      in
      (* [Object] extracts the raw JSON of a JS object or array; anything else is rejected. *)
      let%bind () = eval_and_print Object "({ a: 1 })" in
      [%expect {| (Ok (Object ((a (Number 1))))) |}];
      let%bind () = eval_and_print Object "[1, 2, 3]" in
      [%expect {| (Ok (Array ((Number 1) (Number 2) (Number 3)))) |}];
      let%bind () = eval_and_print Object "42" in
      [%expect
        {| (Error ("unexpected JS result" (expected "an object") (obj (Number 42)))) |}];
      let%bind () = eval_and_print Object "null" in
      [%expect {| (Error ("unexpected JS result" (expected "an object") (obj Null))) |}];
      (* [Undefined] is the dedicated witness for an undefined result. *)
      let%bind () = eval_and_print Undefined "undefined" in
      [%expect {| (Ok ()) |}];
      let%bind () = eval_and_print Undefined "42" in
      [%expect
        {| (Error ("unexpected JS result" (expected undefined) (obj (Number 42)))) |}];
      (* A thrown exception -> Error; the stack's line:col is masked to stay stable. *)
      let%bind () = eval_and_print Object "(function(){ throw new Error('boom'); })()" in
      [%expect
        {|
        (Error
         ("JS evaluation raised" (text Uncaught)
          (description
           ( "Error: boom\
            \n    at <anonymous>:LINE:COL\
            \n    at <anonymous>:LINE:COL"))))
        |}];
      let%bind () = eval_and_print Int "1 + 1" in
      [%expect {| (Ok 2) |}];
      let%bind () = eval_and_print Int "'hi'" in
      [%expect
        {| (Error ("unexpected JS result" (expected "an int") (obj (String hi)))) |}];
      let%bind () = eval_and_print String "'hi'" in
      [%expect {| (Ok hi) |}];
      let%bind () = eval_and_print Bool "1 < 2" in
      [%expect {| (Ok true) |}];
      (* [String_opt] = a string, or null -> None; strict about undefined and non-strings. *)
      let%bind () = eval_and_print String_opt "'hi'" in
      [%expect {| (Ok (hi)) |}];
      let%bind () = eval_and_print String_opt "null" in
      [%expect {| (Ok ()) |}];
      let%bind () = eval_and_print String_opt "undefined" in
      [%expect
        {|
        (Error
         ("unexpected JS result" (expected "a string or null") (obj Undefined)))
        |}];
      let%bind () = eval_and_print String_opt "42" in
      [%expect
        {|
        (Error
         ("unexpected JS result" (expected "a string or null") (obj (Number 42))))
        |}];
      return ())
  in
  [%expect
    {| 1970-01-01 00:00:00.000000Z ("Websocket closed"(reason Normal_closure)(msg"Pipe was closed")) |}];
  return ()
;;

(* Evaluate one representative expression for every shape [Runtime.evaluate] can produce
   and print the parsed [Result.t] (or the CDP/parse error). This is the catalogue of how
   each JS value type comes back through [returnByValue] — several are non-obvious. *)
let%expect_test "Runtime.evaluate result shapes" =
  let%bind () =
    with_page_on_html ~html:"<html><body></body></html>" ~f:(fun page ->
      (* Show the JSON each RPC sends and receives (logged by the connection at [Debug]).
         Scrubbed of per-run ids; restored before teardown so other tests are unaffected. *)
      let orig_level = Log.Global.level () in
      let orig_output = Log.Global.get_output () in
      Log.Global.set_level `Debug;
      Log.Global.set_output
        [ Log.Output.create
            ~flush:(fun () -> return ())
            (fun msgs ->
               Queue.iter msgs ~f:(fun msg ->
                 let line = Log.Message.to_write_only_text msg in
                 (* keep only command request/response (which carry an "id"); async page events
                 arrive at non-deterministic times and would make the output flaky *)
                 if String.is_substring line ~substring:{|\"id\":|}
                 then print_endline (mask_wire_log line));
               return ())
        ];
      let eval_raw expr =
        let%bind result =
          Page.call
            page
            Evaluate.method_
            { Evaluate.Params.expression = expr
            ; return_by_value = true
            ; await_promise = true
            }
        in
        (* flush the wire logs so they print before the parsed result, in order *)
        let%map () = Log.Global.flushed () in
        print_string [%string "%{expr} => "];
        print_sexp_masked [%sexp (result : Evaluate.Result.t Or_error.t)]
      in
      (* primitives map to their obvious by-value forms *)
      let%bind () = eval_raw "42" in
      [%expect
        {|
        1970-01-01 00:00:00.000000Z Debug (cdp-send(json"{\"id\":4,\"sessionId\":\"<id>\",\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":\"42\",\"returnByValue\":true,\"awaitPromise\":true}}"))
        1970-01-01 00:00:00.000000Z Debug (cdp-recv(json"{\"id\":4,\"result\":{\"result\":{\"type\":\"number\",\"value\":42,\"description\":\"42\"}},\"sessionId\":\"<id>\"}"))
        42 => (Ok (Returned (Number 42)))
        |}];
      let%bind () = eval_raw "true" in
      [%expect
        {|
        1970-01-01 00:00:00.000000Z Debug (cdp-send(json"{\"id\":5,\"sessionId\":\"<id>\",\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":\"true\",\"returnByValue\":true,\"awaitPromise\":true}}"))
        1970-01-01 00:00:00.000000Z Debug (cdp-recv(json"{\"id\":5,\"result\":{\"result\":{\"type\":\"boolean\",\"value\":true}},\"sessionId\":\"<id>\"}"))
        true => (Ok (Returned (Boolean true)))
        |}];
      let%bind () = eval_raw "'hi'" in
      [%expect
        {|
        1970-01-01 00:00:00.000000Z Debug (cdp-send(json"{\"id\":6,\"sessionId\":\"<id>\",\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":\"'hi'\",\"returnByValue\":true,\"awaitPromise\":true}}"))
        1970-01-01 00:00:00.000000Z Debug (cdp-recv(json"{\"id\":6,\"result\":{\"result\":{\"type\":\"string\",\"value\":\"hi\"}},\"sessionId\":\"<id>\"}"))
        'hi' => (Ok (Returned (String hi)))
        |}];
      let%bind () = eval_raw "null" in
      [%expect
        {|
        1970-01-01 00:00:00.000000Z Debug (cdp-send(json"{\"id\":7,\"sessionId\":\"<id>\",\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":\"null\",\"returnByValue\":true,\"awaitPromise\":true}}"))
        1970-01-01 00:00:00.000000Z Debug (cdp-recv(json"{\"id\":7,\"result\":{\"result\":{\"type\":\"object\",\"subtype\":\"null\",\"value\":null}},\"sessionId\":\"<id>\"}"))
        null => (Ok (Returned Null))
        |}];
      let%bind () = eval_raw "undefined" in
      [%expect
        {|
        1970-01-01 00:00:00.000000Z Debug (cdp-send(json"{\"id\":8,\"sessionId\":\"<id>\",\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":\"undefined\",\"returnByValue\":true,\"awaitPromise\":true}}"))
        1970-01-01 00:00:00.000000Z Debug (cdp-recv(json"{\"id\":8,\"result\":{\"result\":{\"type\":\"undefined\"}},\"sessionId\":\"<id>\"}"))
        undefined => (Ok (Returned Undefined))
        |}];
      (* numbers with no JSON representation arrive as [unserializableValue] *)
      let%bind () = eval_raw "1/0" in
      [%expect
        {|
        1970-01-01 00:00:00.000000Z Debug (cdp-send(json"{\"id\":9,\"sessionId\":\"<id>\",\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":\"1/0\",\"returnByValue\":true,\"awaitPromise\":true}}"))
        1970-01-01 00:00:00.000000Z Debug (cdp-recv(json"{\"id\":9,\"result\":{\"result\":{\"type\":\"number\",\"unserializableValue\":\"Infinity\",\"description\":\"Infinity\"}},\"sessionId\":\"<id>\"}"))
        1/0 => (Ok (Returned (Number INF)))
        |}];
      let%bind () = eval_raw "-1/0" in
      [%expect
        {|
        1970-01-01 00:00:00.000000Z Debug (cdp-send(json"{\"id\":10,\"sessionId\":\"<id>\",\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":\"-1/0\",\"returnByValue\":true,\"awaitPromise\":true}}"))
        1970-01-01 00:00:00.000000Z Debug (cdp-recv(json"{\"id\":10,\"result\":{\"result\":{\"type\":\"number\",\"unserializableValue\":\"-Infinity\",\"description\":\"-Infinity\"}},\"sessionId\":\"<id>\"}"))
        -1/0 => (Ok (Returned (Number -INF)))
        |}];
      let%bind () = eval_raw "0/0" in
      [%expect
        {|
        1970-01-01 00:00:00.000000Z Debug (cdp-send(json"{\"id\":11,\"sessionId\":\"<id>\",\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":\"0/0\",\"returnByValue\":true,\"awaitPromise\":true}}"))
        1970-01-01 00:00:00.000000Z Debug (cdp-recv(json"{\"id\":11,\"result\":{\"result\":{\"type\":\"number\",\"unserializableValue\":\"NaN\",\"description\":\"NaN\"}},\"sessionId\":\"<id>\"}"))
        0/0 => (Ok (Returned (Number NAN)))
        |}];
      let%bind () = eval_raw "-0" in
      [%expect
        {|
        1970-01-01 00:00:00.000000Z Debug (cdp-send(json"{\"id\":12,\"sessionId\":\"<id>\",\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":\"-0\",\"returnByValue\":true,\"awaitPromise\":true}}"))
        1970-01-01 00:00:00.000000Z Debug (cdp-recv(json"{\"id\":12,\"result\":{\"result\":{\"type\":\"number\",\"unserializableValue\":\"-0\",\"description\":\"-0\"}},\"sessionId\":\"<id>\"}"))
        -0 => (Ok (Returned (Number -0)))
        |}];
      (* a bigint arrives as [unserializableValue] "<n>n" *)
      let%bind () = eval_raw "10n" in
      [%expect
        {|
        1970-01-01 00:00:00.000000Z Debug (cdp-send(json"{\"id\":13,\"sessionId\":\"<id>\",\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":\"10n\",\"returnByValue\":true,\"awaitPromise\":true}}"))
        1970-01-01 00:00:00.000000Z Debug (cdp-recv(json"{\"id\":13,\"result\":{\"result\":{\"type\":\"bigint\",\"unserializableValue\":\"10n\",\"description\":\"10n\"}},\"sessionId\":\"<id>\"}"))
        10n => (Ok (Returned (Bigint 10)))
        |}];
      (* an object and an array both arrive as [type: object] carrying their JSON value;
         CDP sends no [subtype] under [returnByValue], so the JSON is all we get *)
      let%bind () = eval_raw "({ a: 1 })" in
      [%expect
        {|
        1970-01-01 00:00:00.000000Z Debug (cdp-send(json"{\"id\":14,\"sessionId\":\"<id>\",\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":\"({ a: 1 })\",\"returnByValue\":true,\"awaitPromise\":true}}"))
        1970-01-01 00:00:00.000000Z Debug (cdp-recv(json"{\"id\":14,\"result\":{\"result\":{\"type\":\"object\",\"value\":{\"a\":1}}},\"sessionId\":\"<id>\"}"))
        ({ a: 1 }) => (Ok (Returned (Object (Object ((a (Number 1)))))))
        |}];
      let%bind () = eval_raw "[1, 2, 3]" in
      [%expect
        {|
        1970-01-01 00:00:00.000000Z Debug (cdp-send(json"{\"id\":15,\"sessionId\":\"<id>\",\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":\"[1, 2, 3]\",\"returnByValue\":true,\"awaitPromise\":true}}"))
        1970-01-01 00:00:00.000000Z Debug (cdp-recv(json"{\"id\":15,\"result\":{\"result\":{\"type\":\"object\",\"value\":[1,2,3]}},\"sessionId\":\"<id>\"}"))
        [1, 2, 3] => (Ok (Returned (Object (Array ((Number 1) (Number 2) (Number 3))))))
        |}];
      (* date/regexp/map/set/error/node all serialize to an empty {} and, with no [subtype],
         are indistinguishable from each other and from a plain empty object *)
      let%bind () = eval_raw "new Date(0)" in
      [%expect
        {|
        1970-01-01 00:00:00.000000Z Debug (cdp-send(json"{\"id\":16,\"sessionId\":\"<id>\",\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":\"new Date(0)\",\"returnByValue\":true,\"awaitPromise\":true}}"))
        1970-01-01 00:00:00.000000Z Debug (cdp-recv(json"{\"id\":16,\"result\":{\"result\":{\"type\":\"object\",\"value\":{}}},\"sessionId\":\"<id>\"}"))
        new Date(0) => (Ok (Returned (Object (Object ()))))
        |}];
      let%bind () = eval_raw "/re/g" in
      [%expect
        {|
        1970-01-01 00:00:00.000000Z Debug (cdp-send(json"{\"id\":17,\"sessionId\":\"<id>\",\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":\"/re/g\",\"returnByValue\":true,\"awaitPromise\":true}}"))
        1970-01-01 00:00:00.000000Z Debug (cdp-recv(json"{\"id\":17,\"result\":{\"result\":{\"type\":\"object\",\"value\":{}}},\"sessionId\":\"<id>\"}"))
        /re/g => (Ok (Returned (Object (Object ()))))
        |}];
      let%bind () = eval_raw "new Map([[1, 2]])" in
      [%expect
        {|
        1970-01-01 00:00:00.000000Z Debug (cdp-send(json"{\"id\":18,\"sessionId\":\"<id>\",\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":\"new Map([[1, 2]])\",\"returnByValue\":true,\"awaitPromise\":true}}"))
        1970-01-01 00:00:00.000000Z Debug (cdp-recv(json"{\"id\":18,\"result\":{\"result\":{\"type\":\"object\",\"value\":{}}},\"sessionId\":\"<id>\"}"))
        new Map([[1, 2]]) => (Ok (Returned (Object (Object ()))))
        |}];
      let%bind () = eval_raw "new Set([1])" in
      [%expect
        {|
        1970-01-01 00:00:00.000000Z Debug (cdp-send(json"{\"id\":19,\"sessionId\":\"<id>\",\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":\"new Set([1])\",\"returnByValue\":true,\"awaitPromise\":true}}"))
        1970-01-01 00:00:00.000000Z Debug (cdp-recv(json"{\"id\":19,\"result\":{\"result\":{\"type\":\"object\",\"value\":{}}},\"sessionId\":\"<id>\"}"))
        new Set([1]) => (Ok (Returned (Object (Object ()))))
        |}];
      (* a function also serializes to {}; we discard the value and decode it as [Function] *)
      let%bind () = eval_raw "(function(){})" in
      [%expect
        {|
        1970-01-01 00:00:00.000000Z Debug (cdp-send(json"{\"id\":20,\"sessionId\":\"<id>\",\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":\"(function(){})\",\"returnByValue\":true,\"awaitPromise\":true}}"))
        1970-01-01 00:00:00.000000Z Debug (cdp-recv(json"{\"id\":20,\"result\":{\"result\":{\"type\":\"function\",\"value\":{}}},\"sessionId\":\"<id>\"}"))
        (function(){}) => (Ok (Returned Function))
        |}];
      (* a thrown Error: bare [text] plus the rich [description] (message + stack) *)
      let%bind () = eval_raw "(function(){ throw new Error('boom'); })()" in
      [%expect
        {|
        1970-01-01 00:00:00.000000Z Debug (cdp-send(json"{\"id\":21,\"sessionId\":\"<id>\",\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":\"(function(){ throw new Error('boom'); })()\",\"returnByValue\":true,\"awaitPromise\":true}}"))
        1970-01-01 00:00:00.000000Z Debug (cdp-recv(json"{\"id\":21,\"result\":{\"result\":{\"type\":\"object\",\"subtype\":\"error\",\"className\":\"Error\",\"description\":\"Error: boom\\n    at <anonymous>:LINE:COL\\n    at <anonymous>:LINE:COL\",\"objectId\":\"<object-id>\"},\"exceptionDetails\":{\"exceptionId\":1,\"text\":\"Uncaught\",\"lineNumber\":0,\"columnNumber\":13,\"scriptId\":\"20\",\"exception\":{\"type\":\"object\",\"subtype\":\"error\",\"className\":\"Error\",\"description\":\"Error: boom\\n    at <anonymous>:LINE:COL\\n    at <anonymous>:LINE:COL\",\"objectId\":\"<object-id>\"}}},\"sessionId\":\"<id>\"}"))
        (function(){ throw new Error('boom'); })() => (Ok
         (Exception (text Uncaught)
          (description
           ( "Error: boom\
            \n    at <anonymous>:LINE:COL\
            \n    at <anonymous>:LINE:COL"))))
        |}];
      (* a thrown primitive has no [exception.description], so only [text] is present *)
      let%bind () = eval_raw "(function(){ throw 'boom'; })()" in
      [%expect
        {|
        1970-01-01 00:00:00.000000Z Debug (cdp-send(json"{\"id\":22,\"sessionId\":\"<id>\",\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":\"(function(){ throw 'boom'; })()\",\"returnByValue\":true,\"awaitPromise\":true}}"))
        1970-01-01 00:00:00.000000Z Debug (cdp-recv(json"{\"id\":22,\"result\":{\"result\":{\"type\":\"string\",\"value\":\"boom\"},\"exceptionDetails\":{\"exceptionId\":2,\"text\":\"Uncaught\",\"lineNumber\":0,\"columnNumber\":13,\"scriptId\":\"21\",\"exception\":{\"type\":\"string\",\"value\":\"boom\"}}},\"sessionId\":\"<id>\"}"))
        (function(){ throw 'boom'; })() => (Ok (Exception (text Uncaught) (description ())))
        |}];
      (* a symbol can't be serialized: CDP errors before we ever decode a result *)
      let%bind () = eval_raw "Symbol('x')" in
      [%expect
        {|
        1970-01-01 00:00:00.000000Z Debug (cdp-send(json"{\"id\":23,\"sessionId\":\"<id>\",\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":\"Symbol('x')\",\"returnByValue\":true,\"awaitPromise\":true}}"))
        1970-01-01 00:00:00.000000Z Debug (cdp-recv(json"{\"id\":23,\"error\":{\"code\":-32000,\"message\":\"Object couldn't be returned by value\"},\"sessionId\":\"<id>\"}"))
        Symbol('x') => (Error
         ("CDP error"
          ((code -32000) (message "Object couldn't be returned by value"))))
        |}];
      Log.Global.set_level orig_level;
      Log.Global.set_output orig_output;
      return ())
  in
  [%expect
    {| 1970-01-01 00:00:00.000000Z ("Websocket closed"(reason Normal_closure)(msg"Pipe was closed")) |}];
  return ()
;;
