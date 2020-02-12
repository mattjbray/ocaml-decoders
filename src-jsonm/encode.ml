type env =
  { encoder : Jsonm.encoder
  ; on_partial : unit -> unit
  }

let make_env ~encoder ?(on_partial = fun () -> failwith "Not expecting `Partial") () =
  { encoder; on_partial }

let make_encoder' x {encoder; on_partial} =
  let rec await () =
    on_partial ();
    match Jsonm.encode encoder `Await with
    | `Ok -> ()
    | `Partial -> await ()
  in
  match Jsonm.encode encoder x with
  | `Ok -> ()
  | `Partial -> await ()

let make_encoder l env = make_encoder' (`Lexeme l) env

type v = env -> unit

let (>>) (v1 : v) (v2 : v) : v = fun env ->
  v1 env;
  v2 env

let iter encode xs : v = fun env ->
  xs |> List.iter (fun x -> encode x env)

let object_start = make_encoder `Os
let name x = make_encoder (`Name x)
let object_end = make_encoder `Oe
let array_start = make_encoder `As
let array_end = make_encoder `Ae
let end_ = make_encoder' `End

module Jsonm_encodeable = struct
  type value = v

  let to_string (_v : value) : string =
    failwith "Not implemented"

  let of_string x : value = make_encoder (`String x)

  let of_int x : value = make_encoder (`Float (float_of_int x))

  let of_float x : value = make_encoder (`Float x)

  let of_bool x : value = make_encoder (`Bool x)

  let null : value = make_encoder `Null

  let of_list (xs : value list) : value =
    array_start >>
    iter (fun x -> x) xs >>
    array_end

  let of_key_value_pairs (xs : (value * value) list) : value =
    object_start >>
    iter (fun (k,v) -> k >> v) xs >>
    object_end
end

include Decoders.Encode.Make(Jsonm_encodeable)

let list encode xs =
  array_start >>
  iter encode xs >>
  array_end

let obj (xs : (string * value) list) : value =
  object_start >>
  iter (fun (k, v) -> name k >> v) xs >>
  object_end
