include Decoders.Xml.S with type value = Ezxmlm.node

val tag_ns : Xmlm.name -> unit decoder

val any_tag_ns : Xmlm.name decoder

val attrs_ns : Xmlm.attribute list decoder

val attr_opt_ns : Xmlm.name -> string option decoder

val attr_ns : Xmlm.name -> string decoder
