module My_result = struct
  type ('good, 'bad) t = ('good, 'bad) Belt.Result.t =
    | Ok of 'good
    | Error of 'bad

  let return x = Ok x

  let map : ('a -> 'b) -> ('a, 'err) t -> ('b, 'err) t =
   fun f x -> Belt.Result.map x f


  let map_err : ('err1 -> 'err2) -> ('a, 'err1) t -> ('a, 'err2) t =
   fun f -> function Ok x -> Ok x | Error e -> Error (f e)


  module Infix = struct
    let ( >|= ) : ('a, 'err) t -> ('a -> 'b) -> ('b, 'err) t = Belt.Result.map

    let ( >>= ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t =
      Belt.Result.flatMap
  end
end

module My_opt = struct
  let return x = Some x

  let map f x = Belt.Option.map x f

  let flat_map f x = Belt.Option.flatMap x f
end

module My_list = struct
  let take i xs = xs |. Belt.List.take i |. Belt.Option.getWithDefault []

  let map f xs = Belt.List.map xs f

  let mapi f xs = Belt.List.mapWithIndex xs f

  let find_map f xs =
    xs
    |. Belt.List.getBy (fun x ->
           match f x with Some _ -> true | None -> false )
    |. Belt.Option.flatMap f


  let fold_left f init xs = Belt.List.reduce xs init f
end
