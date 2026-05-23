open! Core
open Jsonaf.Export

module Browser = struct
  module Close = struct
    let method_ =
      Method.create
        ~name:"Browser.close"
        ~params_to_jsonaf:Method.empty_params
        ~result_of_jsonaf:Method.ignore_result
    ;;
  end
end

module Target = struct
  module Create_target = struct
    module Params = struct
      type t = { url : string } [@@deriving jsonaf_of]
    end

    module Result = struct
      type t = { target_id : string [@key "targetId"] }
      [@@deriving of_jsonaf] [@@allow_extra_fields]
    end

    let method_ =
      Method.create
        ~name:"Target.createTarget"
        ~params_to_jsonaf:[%jsonaf_of: Params.t]
        ~result_of_jsonaf:[%of_jsonaf: Result.t]
    ;;
  end

  module Attach_to_target = struct
    module Params = struct
      type t =
        { target_id : string [@key "targetId"]
        ; flatten : bool
        }
      [@@deriving jsonaf_of]
    end

    module Result = struct
      type t = { session_id : string [@key "sessionId"] }
      [@@deriving of_jsonaf] [@@allow_extra_fields]
    end

    let method_ =
      Method.create
        ~name:"Target.attachToTarget"
        ~params_to_jsonaf:[%jsonaf_of: Params.t]
        ~result_of_jsonaf:[%of_jsonaf: Result.t]
    ;;
  end

  module Close_target = struct
    module Params = struct
      type t = { target_id : string [@key "targetId"] } [@@deriving jsonaf_of]
    end

    let method_ =
      Method.create
        ~name:"Target.closeTarget"
        ~params_to_jsonaf:[%jsonaf_of: Params.t]
        ~result_of_jsonaf:Method.ignore_result
    ;;
  end
end

module Page = struct
  module Enable = struct
    let method_ =
      Method.create
        ~name:"Page.enable"
        ~params_to_jsonaf:Method.empty_params
        ~result_of_jsonaf:Method.ignore_result
    ;;
  end

  module Navigate = struct
    module Params = struct
      type t = { url : string } [@@deriving jsonaf_of]
    end

    module Result = struct
      type t = { frame_id : string [@key "frameId"] }
      [@@deriving of_jsonaf] [@@allow_extra_fields]
    end

    let method_ =
      Method.create
        ~name:"Page.navigate"
        ~params_to_jsonaf:[%jsonaf_of: Params.t]
        ~result_of_jsonaf:[%of_jsonaf: Result.t]
    ;;
  end

  module Capture_screenshot = struct
    module Result = struct
      type t = { data : string } [@@deriving of_jsonaf] [@@allow_extra_fields]
    end

    let method_ =
      Method.create
        ~name:"Page.captureScreenshot"
        ~params_to_jsonaf:Method.empty_params
        ~result_of_jsonaf:[%of_jsonaf: Result.t]
    ;;
  end

  let load_event_fired_method_name = "Page.loadEventFired"
end

module Remote_object_type = struct
  (* snake_case capitalization produces the lowercase CDP wire strings. *)
  module T = struct
    type t =
      | Object
      | Function
      | Undefined
      | String
      | Number
      | Boolean
      | Symbol
      | Bigint
    [@@deriving enumerate, sexp_of, string ~capitalize:"snake_case"]
  end

  include T
  include Jsonaf.Jsonafable.Of_stringable (T)
end

module Remote_object = struct
  (* A CDP RemoteObject decoded by its [type]: serializable JS types carry an OCaml value;
     the rest have no by-value representation. With [returnByValue], CDP sends no [subtype],
     so kinds like Date/Map/Set/Error all collapse to [Object] of an empty JSON object. *)
  type t =
    | Null
    | Undefined
    | Boolean of bool
    | Number of float
    | String of string
    | Object of Jsonaf.t
    | Function
    | Symbol
    | Bigint of Bigint.t
  [@@deriving sexp_of]

  (* Structural decode of a raw JSON value — one with no CDP [type] beside it, e.g. a value
     nested inside a returned object. JSON can't represent undefined/function/symbol/bigint. *)
  let t_of_jsonaf : Jsonaf.t -> t = function
    | `Null -> Null
    | `True -> Boolean true
    | `False -> Boolean false
    | `Number n -> Number (Float.of_string n)
    | `String s -> String s
    | (`Object _ | `Array _) as json -> Object json
  ;;
end

module Runtime = struct
  module Evaluate = struct
    module Params = struct
      type t =
        { expression : string
        ; return_by_value : bool [@key "returnByValue"]
        ; await_promise : bool [@key "awaitPromise"]
        }
      [@@deriving jsonaf_of]
    end

    module Result = struct
      (* An evaluation either returns a RemoteObject or throws. [t] interprets the wire
         shape, not a structural mirror of it, so ppx_jsonaf_conv can't derive it directly;
         it derives the wire shape below and interprets it in [of_jsonaf]. *)
      type t =
        | Returned of Remote_object.t
        | Exception of
            { text : string
            ; description : string option
            }
      [@@deriving sexp_of]

      module Wire = struct
        module Remote_object = struct
          type t =
            { type_ : Remote_object_type.t [@key "type"]
            ; value : Jsonaf.t option [@jsonaf.option]
            ; unserializable_value : string option
                  [@key "unserializableValue"] [@jsonaf.option]
            }
          [@@deriving of_jsonaf] [@@allow_extra_fields]
        end

        module Exception_details = struct
          module Exception = struct
            type t = { description : string option [@jsonaf.option] }
            [@@deriving of_jsonaf] [@@allow_extra_fields]
          end

          type t =
            { text : string
            ; exception_ : Exception.t option [@key "exception"] [@jsonaf.option]
            }
          [@@deriving of_jsonaf] [@@allow_extra_fields]
        end

        type t =
          { result : Remote_object.t option [@jsonaf.option]
          ; exception_details : Exception_details.t option
                [@key "exceptionDetails"] [@jsonaf.option]
          }
        [@@deriving of_jsonaf] [@@allow_extra_fields]
      end

      (* Interpret the evaluate response. A returned RemoteObject is built from the only
         [(type, value, unserializableValue)] combinations CDP produces; any other shape
         is one we don't model. *)
      let of_jsonaf json =
        match [%of_jsonaf: Wire.t] json with
        (* a throw reports [exceptionDetails] (and the thrown value as [result], which we
           ignore in favor of the structured exception) *)
        | { result = _; exception_details = Some { text; exception_ } } ->
          Exception
            { text
            ; description = Option.bind exception_ ~f:(fun { description } -> description)
            }
        | { result = None; exception_details = None } ->
          Ppx_jsonaf_conv_lib.Jsonaf_conv.of_jsonaf_error
            "Runtime.evaluate response had no result"
            json
        | { result = Some remote_object; exception_details = None } ->
          Returned
            (match remote_object with
             (* serializable types (incl. [null] and arrays/objects) decode structurally
                from their by-value JSON *)
             | { type_ = Boolean | String | Object | Number
               ; value = Some value
               ; unserializable_value = None
               } -> Remote_object.t_of_jsonaf value
             (* a function serializes to an empty object we discard *)
             | { type_ = Function; value = Some _; unserializable_value = None } ->
               Function
             (* a [number] with no JSON representation arrives as [unserializableValue];
                [Float.of_string] parses "Infinity"/"-Infinity"/"NaN"/"-0" directly *)
             | { type_ = Number; value = None; unserializable_value = Some s } ->
               Number (Float.of_string s)
             (* a [bigint] always arrives as [unserializableValue], e.g. "10n" *)
             | { type_ = Bigint; value = None; unserializable_value = Some s } ->
               Bigint (Bigint.of_string (String.chop_suffix_if_exists s ~suffix:"n"))
             (* types with no by-value representation carry neither field *)
             | { type_ = Undefined; value = None; unserializable_value = None } ->
               Undefined
             | { type_ = Symbol; value = None; unserializable_value = None } -> Symbol
             | { type_ =
                   ( Boolean
                   | String
                   | Object
                   | Number
                   | Bigint
                   | Undefined
                   | Function
                   | Symbol )
               ; value = _
               ; unserializable_value = _
               } ->
               Ppx_jsonaf_conv_lib.Jsonaf_conv.of_jsonaf_error
                 "unexpected RemoteObject shape"
                 json)
      ;;
    end

    let method_ =
      Method.create
        ~name:"Runtime.evaluate"
        ~params_to_jsonaf:[%jsonaf_of: Params.t]
        ~result_of_jsonaf:Result.of_jsonaf
    ;;
  end
end

module Input = struct
  module Dispatch_key_event = struct
    module Event_type = struct
      module T = struct
        type t =
          | Key_down
          | Key_up
        [@@deriving string ~capitalize:"camelCase"]
      end

      include T
      include Jsonaf.Jsonafable.Of_stringable (T)
    end

    module Params = struct
      type t =
        { type_ : Event_type.t [@key "type"]
        ; modifiers : int
        ; key : string
        ; code : string
        ; windows_virtual_key_code : int [@key "windowsVirtualKeyCode"]
        ; text : string option [@jsonaf.option]
        }
      [@@deriving jsonaf_of]
    end

    let method_ =
      Method.create
        ~name:"Input.dispatchKeyEvent"
        ~params_to_jsonaf:[%jsonaf_of: Params.t]
        ~result_of_jsonaf:Method.ignore_result
    ;;
  end

  module Insert_text = struct
    module Params = struct
      type t = { text : string } [@@deriving jsonaf_of]
    end

    let method_ =
      Method.create
        ~name:"Input.insertText"
        ~params_to_jsonaf:[%jsonaf_of: Params.t]
        ~result_of_jsonaf:Method.ignore_result
    ;;
  end
end
