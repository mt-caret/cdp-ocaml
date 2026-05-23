open! Core
module Remote_object = Protocol.Remote_object

type _ t =
  | String : string t
  | Int : int t
  | Bool : bool t
  | String_opt : string option t
  | Object : Jsonaf.t t
  | Undefined : unit t

let decode : type a. a t -> Remote_object.t -> a Or_error.t =
  fun witness obj ->
  let mismatch expected =
    Or_error.error_s
      [%message "unexpected JS result" (expected : string) (obj : Remote_object.t)]
  in
  match witness with
  | String ->
    (match obj with
     | String s -> Ok s
     | Null | Undefined | Boolean _ | Number _ | Object _ | Function | Symbol | Bigint _
       -> mismatch "a string")
  | Bool ->
    (match obj with
     | Boolean b -> Ok b
     | Null | Undefined | Number _ | String _ | Object _ | Function | Symbol | Bigint _ ->
       mismatch "a bool")
  | Int ->
    (match obj with
     | Number f when Float.is_integer f -> Ok (Int.of_float f)
     | Number _
     | Null
     | Undefined
     | Boolean _
     | String _
     | Object _
     | Function
     | Symbol
     | Bigint _ -> mismatch "an int")
  | String_opt ->
    (match obj with
     | String s -> Ok (Some s)
     | Null -> Ok None
     | Undefined | Boolean _ | Number _ | Object _ | Function | Symbol | Bigint _ ->
       mismatch "a string or null")
  | Undefined ->
    (match obj with
     | Undefined -> Ok ()
     | Null | Boolean _ | Number _ | String _ | Object _ | Function | Symbol | Bigint _ ->
       mismatch "undefined")
  | Object ->
    (match obj with
     | Object json -> Ok json
     | Null | Undefined | Boolean _ | Number _ | String _ | Function | Symbol | Bigint _
       -> mismatch "an object")
;;
