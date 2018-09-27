#ifdef IS_BUCKLESCRIPT
module Result = struct
  type ('good, 'bad) t = ('good, 'bad) Belt.Result.t = | Ok of 'good | Error of 'bad

  let return x = Ok x
  let map : ('a -> 'b) -> ('a, 'err) t -> ('b, 'err) t = fun f x -> Belt.Result.map x f
  let map_err : ('err1 -> 'err2) -> ('a, 'err1) t -> ('a, 'err2) t = fun f -> function
    | Ok x -> Ok x
    | Error e -> Error (f e)

  module Infix = struct
    let ( >|= ) : ('a, 'err) t -> ('a -> 'b) -> ('b, 'err) t = Belt.Result.map
    let ( >>= ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t = Belt.Result.flatMap
  end
end

module Opt = struct
  let return x = Some x
  let flat_map f x = Belt.Option.flatMap x f
end

module My_list = struct
  let take i xs =
    xs
    |. Belt.List.take i
    |. Belt.Option.getWithDefault []

  let map = List.map
  let mapi = List.mapi
  let find_map f xs =
    xs
    |. Belt.List.getBy (fun x ->
        match f x with
        | Some _ -> true
        | None -> false)
    |. Belt.Option.flatMap f
end
#else
module Result = struct
  type ('good, 'bad) t = ('good, 'bad) CCResult.result = | Ok of 'good | Error of 'bad

  let return x = Ok x
  let map : ('a -> 'b) -> ('a, 'err) t -> ('b, 'err) t = CCResult.map
  let map_err : ('err1 -> 'err2) -> ('a, 'err1) t -> ('a, 'err2) t = CCResult.map_err

  module Infix = struct
    let ( >|= ) : ('a, 'err) t -> ('a -> 'b) -> ('b, 'err) t =  CCResult.Infix.(>|=)
    let ( >>= ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t = CCResult.Infix.(>>=)
  end
end

module Opt = struct
  let return = CCOpt.return
  let flat_map = CCOpt.flat_map
end

module My_list = struct
  let take = CCList.take
  let map = CCList.map
  let mapi = CCList.mapi
  let find_map = CCList.find_map
end
#endif

type ('good, 'bad) result = ('good, 'bad) Result.t = Ok of 'good | Error of 'bad
