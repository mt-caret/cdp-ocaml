open! Core

type ('p, 'r, 'scope) t =
  { name : string
  ; params_to_jsonaf : 'p -> Jsonaf.t
  ; result_of_jsonaf : Jsonaf.t -> 'r
  }

let create ~name ~params_to_jsonaf ~result_of_jsonaf =
  { name; params_to_jsonaf; result_of_jsonaf }
;;

let empty_params () = `Object []
let ignore_result (_ : Jsonaf.t) = ()
