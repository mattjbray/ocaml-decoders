module Result = struct
  let map f = function
    | Ok x -> Ok (f x)
    | Error e -> Error e

  let map_error f = function
    | Ok x -> Ok x
    | Error e -> Error (f e)

  let bind f = function
    | Ok x -> (f x)
    | Error e -> Error e

  module Infix = struct
    let (>>|) x f = map f x
    let (>>=) x f = bind f x
  end
end
