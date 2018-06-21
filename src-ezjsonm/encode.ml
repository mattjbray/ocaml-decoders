module Ezjsonm_encodeable = struct
  type value = Ezjsonm.value

  let to_string = function
    | `Null -> "null"
    | `Bool bool -> string_of_bool bool
    | `Float float -> string_of_float float
    | `String string -> Printf.sprintf "%S" string
    | (`A _ | `O _) as json -> Ezjsonm.(to_string json)

  let of_string x = `String x
  let of_int x = `Float (float_of_int x)
  let of_float x = `Float x
  let of_bool x = `Bool x
  let null = `Null

  let of_list xs = `A xs
  let of_key_value_pairs xs =
    `O
      (xs
       |> CCList.filter_map (fun (k, v) ->
           match k with
           | `String k -> Some (k, v)
           | _ -> None))
end

include Decoders.Encode.Make(Ezjsonm_encodeable)
