type value =
  [ `El of Dom.element
  | `Data of Dom.text
  ]

module Decode : sig
  include Decoders.Xml.Decode with type value = value
end

module Encode : sig
  include Decoders.Xml.Encode with type value = value
end
