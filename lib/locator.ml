open! Core
open! Async
open Jsonaf.Export
module Remote_object = Protocol.Remote_object

type t =
  | Css of string
  | Text of string
  | Has_text of string
  | Nth of int (* [-1] means "last" *)
[@@deriving sexp_of]

let js_string s = Jsonaf.to_string (`String s)
let css s = Css s
let text s = Text s
let placeholder s = Css [%string {|[placeholder=%{js_string s}]|}]
let has_text s = Has_text s
let nth i = Nth i
let first = Nth 0
let last = Nth (-1)

(* The default bounded auto-wait: ~5s (100 polls of 50ms). *)
let default_attempts = 100
let default_interval = Time_ns.Span.of_int_ms 50

let step_to_js = function
  | Css sel ->
    [%string
      {|els = els.flatMap(r => Array.from(r.querySelectorAll(%{js_string sel})));|}]
  | Text t ->
    let s = js_string t in
    (* innermost elements whose text contains [t] — avoids matching every ancestor *)
    [%string
      {|els = els.flatMap(r => Array.from(r.querySelectorAll('*')).filter(e => e.textContent.includes(%{s}) && !Array.from(e.children).some(c => c.textContent.includes(%{s}))));|}]
  | Has_text t ->
    [%string {|els = els.filter(e => e.textContent.includes(%{js_string t}));|}]
  | Nth -1 -> {|els = els.length ? [els[els.length - 1]] : [];|}
  | Nth i -> [%string {|els = (els[%{i#Int}] !== undefined) ? [els[%{i#Int}]] : [];|}]
;;

let to_js chain =
  let body =
    Nonempty_list.to_list chain
    |> List.map ~f:step_to_js
    |> List.cons "let els = [document];"
    |> String.concat ~sep:"\n  "
  in
  [%string
    {|(function(){
  %{body}
  return els;
})()|}]
;;

let describe chain = Sexp.to_string_hum [%sexp (Nonempty_list.to_list chain : t list)]

(* One-shot query: [body] is a JS expression evaluating against the resolved [els] array. *)
let query page chain ~witness ~body =
  Page.eval
    page
    witness
    [%string
      {|(function(){
  const els = %{to_js chain};
  return (%{body});
})()|}]
;;

(* The wrapper object the resolver IIFE returns. Deriving [of_jsonaf] decodes [__cdp_value]
   into a [Remote_object.t] via its structural converter. *)
module Found = struct
  type t =
    { found : bool [@key "__cdp_found"]
    ; value : Remote_object.t option [@key "__cdp_value"] [@jsonaf.option]
    }
  [@@deriving of_jsonaf] [@@allow_extra_fields]
end

(* Auto-wait for a match, then evaluate [action] against [el] (= [els[0]]), returning the
   action's value. Retries only the "no element yet" case; a JS exception fails fast. *)
let act_on_first page chain ~action : Remote_object.t Deferred.Or_error.t =
  let expression =
    [%string
      {|(function(){
  const els = %{to_js chain};
  if (els.length === 0) return { __cdp_found: false };
  const el = els[0];
  return { __cdp_found: true, __cdp_value: (%{action}) };
})()|}]
  in
  Deferred.repeat_until_finished default_attempts (fun attempts_left ->
    match%bind Page.eval page Js_type.Object expression with
    | Error _ as err -> return (`Finished err)
    | Ok wrapper ->
      let%tydi { found; value } = [%of_jsonaf: Found.t] wrapper in
      (match found with
       | true ->
         (* an action that produced no value (e.g. omitted [__cdp_value]) reads as
            [undefined] *)
         return (`Finished (Ok (Option.value value ~default:Remote_object.Undefined)))
       | false ->
         (match attempts_left with
          | 0 ->
            return
              (`Finished
                  (Or_error.error_s
                     [%message "no element matched locator" ~steps:(describe chain)]))
          | n ->
            let%map () = Clock_ns.after default_interval in
            `Repeat (n - 1))))
;;

let act_unit page chain ~action =
  let%map.Deferred.Or_error (_ : Remote_object.t) = act_on_first page chain ~action in
  ()
;;

let act_decode page chain ~witness ~action =
  let%bind.Deferred.Or_error obj = act_on_first page chain ~action in
  Deferred.return (Js_type.decode witness obj)
;;

let count page chain = query page chain ~witness:Js_type.Int ~body:"els.length"

let is_visible page chain =
  query
    page
    chain
    ~witness:Js_type.Bool
    ~body:
      "els.length === 0 ? false : (els[0].offsetWidth > 0 || els[0].offsetHeight > 0 || \
       els[0].getClientRects().length > 0)"
;;

let inner_text page chain =
  act_decode page chain ~witness:Js_type.String ~action:"el.innerText"
;;

let text_content page chain =
  act_decode page chain ~witness:Js_type.String_opt ~action:"el.textContent"
;;

let input_value page chain =
  act_decode page chain ~witness:Js_type.String ~action:"el.value"
;;

let get_attribute page chain name =
  act_decode
    page
    chain
    ~witness:Js_type.String_opt
    ~action:[%string "el.getAttribute(%{js_string name})"]
;;

let click page chain =
  act_unit page chain ~action:"(el.scrollIntoView(), el.click(), null)"
;;

let fill page chain ~text =
  act_unit
    page
    chain
    ~action:
      [%string
        {|(el.focus(), el.value = %{js_string text}, el.dispatchEvent(new Event('input', { bubbles: true })), el.dispatchEvent(new Event('change', { bubbles: true })), null)|}]
;;

let focus page chain = act_unit page chain ~action:"(el.focus(), null)"
let blur page chain = act_unit page chain ~action:"(el.blur(), null)"

let hover page chain =
  act_unit
    page
    chain
    ~action:
      "(el.dispatchEvent(new MouseEvent('mouseover', { bubbles: true })), \
       el.dispatchEvent(new MouseEvent('mousemove', { bubbles: true })), null)"
;;

let press page chain ?modifiers key =
  let%bind.Deferred.Or_error () = focus page chain in
  Page.press page ?modifiers key
;;

let type_text page chain text =
  let%bind.Deferred.Or_error () = focus page chain in
  Page.type_text page text
;;

let wait_for page chain state =
  let check () =
    match state with
    | `Attached ->
      let%map.Deferred.Or_error n = count page chain in
      n > 0
    | `Detached ->
      let%map.Deferred.Or_error n = count page chain in
      n = 0
    | `Visible -> is_visible page chain
    | `Hidden ->
      let%map.Deferred.Or_error visible = is_visible page chain in
      not visible
  in
  Deferred.repeat_until_finished default_attempts (fun attempts_left ->
    match%bind check () with
    | Error _ as err -> return (`Finished err)
    | Ok true -> return (`Finished (Ok ()))
    | Ok false ->
      (match attempts_left with
       | 0 ->
         return
           (`Finished
               (Or_error.error_s [%message "wait_for: timed out" ~steps:(describe chain)]))
       | n ->
         let%map () = Clock_ns.after default_interval in
         `Repeat (n - 1)))
;;

let count_exn page chain = count page chain >>| ok_exn
let is_visible_exn page chain = is_visible page chain >>| ok_exn
let inner_text_exn page chain = inner_text page chain >>| ok_exn
let text_content_exn page chain = text_content page chain >>| ok_exn
let input_value_exn page chain = input_value page chain >>| ok_exn
let get_attribute_exn page chain name = get_attribute page chain name >>| ok_exn
let click_exn page chain = click page chain >>| ok_exn
let fill_exn page chain ~text = fill page chain ~text >>| ok_exn
let focus_exn page chain = focus page chain >>| ok_exn
let blur_exn page chain = blur page chain >>| ok_exn
let hover_exn page chain = hover page chain >>| ok_exn
let press_exn page chain ?modifiers key = press page chain ?modifiers key >>| ok_exn
let type_text_exn page chain text = type_text page chain text >>| ok_exn
let wait_for_exn page chain state = wait_for page chain state >>| ok_exn
