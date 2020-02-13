(** Streaming encoding using [Jsonm].

    Example usage:

    {[
      module E = Decoders_jsonm.Encode

      let run_encoder out_channel (encode : t E.encoder) (x : t) =
        let dst = `Channel out_channel in
        let encoder = Jsonm.encoder ~minify:true dst in
        let env = E.make_env ~encoder () in
        encode x env
    ]}
*)

type env

val make_env : encoder:Jsonm.encoder -> ?on_partial:(unit -> unit) -> unit -> env

include Decoders.Encode.S with type value = env -> unit

(** {2 Low-level combinators}

    Assuming we have:

    {[
      type member
      val member : member encoder
    ]}

    And a type [x]:

    {[
      type x =
        { id : string
        ; members : member list
        }
    ]}

    An encoder for [x] might look like this:

    {[
      let x_encoder x =
        object_start >>

        name "id" >>
        string x.id >>

        name "members" >>
        array_start >>
        iter member x.members >>
        array_end >>

        object_end
    ]}
*)

val (>>) : value -> value -> value
val iter : 'a encoder -> 'a list -> value

val object_start : value
val name : string -> value
val object_end : value
val array_start : value
val array_end : value
val end_ : value
