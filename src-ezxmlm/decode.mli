include Decoders.Xml.S with type value = Ezxmlm.node

val tag_ns : Xmlm.name -> 'a tag_decoder -> 'a decoder

val any_tag_ns : (Xmlm.name -> 'a tag_decoder) -> 'a decoder

val attrs_ns : Xmlm.attribute list tag_decoder

val attr_opt_ns : Xmlm.name -> string option tag_decoder

val attr_ns : Xmlm.name -> string tag_decoder
