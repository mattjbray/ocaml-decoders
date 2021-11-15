type ('i, 'o, 'e) t = 'i -> ('o, 'e) result

let pure x : ('i, 'o, 'e) t = fun _i -> Ok x

let fail e : ('i, 'o, 'e) t = fun _i -> Error e

let of_result = function Ok o -> pure o | Error e -> fail e

let bind (f : 'a -> ('i, 'b, 'e) t) (x : ('i, 'a, 'e) t) : ('i, 'b, 'e) t =
 fun i -> match x i with Ok y -> f y i | Error e -> Error e


let map (f : 'a -> 'b) (x : ('i, 'a, 'e) t) : ('i, 'b, 'e) t =
 fun i -> match x i with Ok y -> Ok (f y) | Error e -> Error e


let map_err (f : 'e1 -> 'e2) (x : ('i, 'o, 'e1) t) : ('i, 'o, 'e2) t =
 fun i -> match x i with Ok y -> Ok y | Error e -> Error (f e)


let apply (f : ('i, 'a -> 'b, 'e) t) (x : ('i, 'a, 'e) t) : ('i, 'b, 'e) t =
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
    type nonrec ('i, 'o, 'e) t = ('i, 'o, 'e) t

    let ( >>= ) = ( >>= )

    let ( >|= ) = ( >|= )

    let[@inline] monoid_product a b = map (fun x y -> (x, y)) a <*> b
  end)
end

let fix (f : ('i, 'a, 'e) t -> ('i, 'a, 'e) t) : ('i, 'a, 'e) t =
  let rec p = lazy (f r)
  and r value = (Lazy.force p) value in
  r


let value : ('i, 'i, 'e) t = fun i -> Ok i

let maybe (x : ('i, 'a, 'e) t) : ('i, 'a option, 'e) t =
 fun i -> match x i with Ok x -> Ok (Some x) | Error _ -> Ok None


let one_of ~combine_errors (xs : ('i, 'o, 'e) t list) : ('i, 'o, 'e) t =
 fun i ->
  let rec aux es = function
    | x :: xs ->
      (match x i with Ok o -> Ok o | Error e -> aux (e :: es) xs)
    | [] ->
        Error (combine_errors (List.rev es))
  in
  aux [] xs


let pick ~combine_errors : ('i, ('i, 'o, 'e) t, 'e) t list -> ('i, 'o, 'e) t =
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
        Error (combine_errors errors)
  in
  go [] decoders


let of_to_opt (to_opt : 'i -> 'o option) fail : ('i, 'o, 'e) t =
 fun i -> match to_opt i with Some o -> Ok o | None -> fail i
