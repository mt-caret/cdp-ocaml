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
