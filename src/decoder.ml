type ('i, 'o) t = 'i -> ('o, 'i Error.t) result

let pure x : ('i, 'o) t = fun _i -> Ok x

let fail msg : ('i, 'o) t = fun i -> Error (Error.make ~context:i msg)

let fail_with e : ('i, 'o) t = fun _i -> Error e

let of_result = function Ok o -> pure o | Error e -> fail_with e

let bind (f : 'a -> ('i, 'b) t) (x : ('i, 'a) t) : ('i, 'b) t =
 fun i -> match x i with Ok y -> f y i | Error e -> Error e


let map (f : 'a -> 'b) (x : ('i, 'a) t) : ('i, 'b) t =
 fun i -> match x i with Ok y -> Ok (f y) | Error e -> Error e


let map_err (f : 'i Error.t -> 'i Error.t) (x : ('i, 'o) t) : ('i, 'o) t =
 fun i -> match x i with Ok y -> Ok y | Error e -> Error (f e)


let apply (f : ('i, 'a -> 'b) t) (x : ('i, 'a) t) : ('i, 'b) t =
 fun i ->
  match f i with
  | Ok f ->
    (match x i with Ok x -> Ok (f x) | Error e -> Error e)
  | Error e ->
      Error e


module Infix = struct
  let[@inline] ( >>= ) x f = bind f x

  let[@inline] ( >|= ) x f = map f x

  let[@inline] ( <*> ) f x = apply f x

  include Shims_let_ops_.Make (struct
    type nonrec ('i, 'o) t = ('i, 'o) t

    let ( >>= ) = ( >>= )

    let ( >|= ) = ( >|= )

    let[@inline] monoid_product a b = map (fun x y -> (x, y)) a <*> b
  end)
end

let fix (f : ('i, 'a) t -> ('i, 'a) t) : ('i, 'a) t =
  let rec p = lazy (f r)
  and r value = (Lazy.force p) value in
  r


let value : ('i, 'i) t = fun i -> Ok i

let maybe (x : ('i, 'a) t) : ('i, 'a option) t =
 fun i -> match x i with Ok x -> Ok (Some x) | Error _ -> Ok None


let one_of (xs : ('i, 'o) t list) : ('i, 'o) t =
 fun i ->
  let rec aux errors = function
    | x :: xs ->
      (match x i with Ok o -> Ok o | Error e -> aux (e :: errors) xs)
    | [] ->
        Error (Error.group (List.rev errors))
  in
  aux [] xs


let pick : ('i, ('i, 'o) t) t list -> ('i, 'o) t =
 fun decoders input ->
  let rec go errors = function
    | decoder :: rest ->
      ( match decoder input with
      | Ok dec ->
        (* use [dec] and drop errors *)
        (match dec input with Ok _ as x -> x | Error e -> Error e)
      | Error error ->
          go (error :: errors) rest )
    | [] ->
        Error (Error.group errors)
  in
  go [] decoders


let of_to_opt (to_opt : 'i -> 'o option) fail : ('i, 'o) t =
 fun i -> match to_opt i with Some o -> Ok o | None -> fail i


let decode_sub v dec = of_result (dec v)
