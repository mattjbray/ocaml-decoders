module My_result = struct
  type ('good, 'bad) t = ('good, 'bad) Belt.Result.t =
    | Ok of 'good
    | Error of 'bad

  let return x = Ok x

  let map : ('a -> 'b) -> ('a, 'err) t -> ('b, 'err) t =
   fun f x -> Belt.Result.map x f


  let map_err : ('err1 -> 'err2) -> ('a, 'err1) t -> ('a, 'err2) t =
   fun f -> function Ok x -> Ok x | Error e -> Error (f e)


  let combine_l (results : ('a, 'e) result list) : ('a list, 'e list) result =
    let rec aux combined = function
      | [] ->
        ( match combined with
        | Ok xs ->
            Ok (List.rev xs)
        | Error es ->
            Error (List.rev es) )
      | result :: rest ->
          let combined =
            match (result, combined) with
            | Ok x, Ok xs ->
                Ok (x :: xs)
            | Error e, Error es ->
                Error (e :: es)
            | Error e, Ok _ ->
                Error [ e ]
            | Ok _, Error es ->
                Error es
          in
          aux combined rest
    in
    aux (Ok []) results


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

  let filter_mapi f l =
    let rec recurse (acc, i) l =
      match l with
      | [] ->
          List.rev acc
      | x :: l' ->
          let acc' = match f i x with None -> acc | Some y -> y :: acc in
          recurse (acc', i + 1) l'
    in
    recurse ([], 0) l


  let filter_map f xs = filter_mapi (fun _i x -> f x) xs

  let find_map f xs =
    xs
    |. Belt.List.getBy (fun x ->
           match f x with Some _ -> true | None -> false )
    |. Belt.Option.flatMap f


  let fold_left f init xs = Belt.List.reduce xs init f
end
