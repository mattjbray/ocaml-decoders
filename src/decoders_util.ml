(** Util module used for native builds (excluded in bs-config.json) *)
module My_result = struct
  type ('good, 'bad) t = ('good, 'bad) CCResult.result = | Ok of 'good | Error of 'bad

  let return x = Ok x
  let map : ('a -> 'b) -> ('a, 'err) t -> ('b, 'err) t = CCResult.map
  let map_err : ('err1 -> 'err2) -> ('a, 'err1) t -> ('a, 'err2) t = CCResult.map_err

  module Infix = struct
    let ( >|= ) : ('a, 'err) t -> ('a -> 'b) -> ('b, 'err) t =  CCResult.Infix.(>|=)
    let ( >>= ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t = CCResult.Infix.(>>=)
  end
end

module My_opt = struct
  let return = CCOpt.return
  let flat_map = CCOpt.flat_map
end

module My_list = struct
  let take = CCList.take
  let map = CCList.map
  let mapi = CCList.mapi
  let find_map = CCList.find_map
end
