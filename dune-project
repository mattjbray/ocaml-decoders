(lang dune 2.0)
(name decoders)
(generate_opam_files true)
(license ISC)
(authors "Matt Bray <mattjbray@gmail.com>")
(maintainers "Matt Bray <mattjbray@gmail.com>")
(source (github "mattjbray/ocaml-decoders"))
(homepage "https://github.com/mattjbray/ocaml-decoders")
(documentation "https://mattjbray.github.io/ocaml-decoders/decoders/")

(package
 (name decoders)
 (synopsis "Elm-inspired decoders for Ocaml")
 (description
   "A combinator library for \"decoding\" JSON-like values into your own Ocaml types, inspired by Elm's `Json.Decode` and `Json.Encode`.")
 (documentation "https://mattjbray.github.io/ocaml-decoders/decoders/decoders/")
 (depends
  (ocaml (>= 4.03.0))
  (dune (>= 1.0))
  (odoc :with-doc)
  (containers :with-test)))

(package
 (name decoders-bencode)
 (authors "Simon Cruanes <simon@imandra.ai>")
 (synopsis "Bencode backend for decoders")
 (description
   "A combinator library for \"decoding\" JSON-like values into your own Ocaml types, inspired by Elm's `Json.Decode` and `Json.Encode`.")
 (documentation "https://mattjbray.github.io/ocaml-decoders/decoders/decoders-bencode/")
 (depends
  (ocaml (>= 4.03.0))
  (dune (>= 1.0))
  decoders
  bencode
  (odoc :with-doc)
  (containers :with-test)
  (ounit :with-test)))

(package
 (name decoders-cbor)
 (synopsis "CBOR backend for decoders")
 (description
   "A combinator library for \"decoding\" JSON-like values into your own Ocaml types, inspired by Elm's `Json.Decode` and `Json.Encode`.")
 (documentation "https://mattjbray.github.io/ocaml-decoders/decoders/decoders-cbor/")
 (depends
  (ocaml (>= 4.03.0))
  (dune (>= 1.0))
  decoders
  cbor
  (odoc :with-doc)
  (containers :with-test)
  (ounit :with-test)))

(package
 (name decoders-ezjsonm)
 (synopsis "Ezjsonm backend for decoders")
 (description
   "A combinator library for \"decoding\" JSON-like values into your own Ocaml types, inspired by Elm's `Json.Decode` and `Json.Encode`.")
 (documentation "https://mattjbray.github.io/ocaml-decoders/decoders/decoders-ezjsonm/")
 (depends
  (ocaml (>= 4.03.0))
  (dune (>= 1.0))
  decoders
  (ezjsonm (>= 0.4.0))
  (odoc :with-doc)
  (containers :with-test)
  (ounit :with-test)))

(package
 (name decoders-jsonm)
 (synopsis "Jsonm backend for decoders")
 (description
   "A combinator library for \"decoding\" JSON-like values into your own Ocaml types, inspired by Elm's `Json.Decode` and `Json.Encode`.")
 (documentation "https://mattjbray.github.io/ocaml-decoders/decoders/decoders-jsonm/")
 (depends
  (ocaml (>= 4.03.0))
  (dune (>= 1.0))
  decoders
  jsonm
  (odoc :with-doc)
  (containers :with-test)
  (ounit :with-test)))

(package
 (name decoders-msgpck)
 (authors "Matt Bray <mattjbray@gmail.com>" "Simon Cruanes <simon@imandra.ai>")
 (synopsis "Msgpck backend for decoders")
 (description
   "A combinator library for \"decoding\" JSON-like values into your own Ocaml types, inspired by Elm's `Json.Decode` and `Json.Encode`.")
 (documentation "https://mattjbray.github.io/ocaml-decoders/decoders/decoders-msgpck/")
 (depends
  (ocaml (>= 4.03.0))
  (dune (>= 1.0))
  decoders
  msgpck
  (odoc :with-doc)
  (containers :with-test)
  (ounit :with-test)))

(package
 (name decoders-sexplib)
 (synopsis "Sexplib backend for decoders")
 (description
   "A combinator library for \"decoding\" JSON-like values into your own Ocaml types, inspired by Elm's `Json.Decode` and `Json.Encode`.")
 (documentation "https://mattjbray.github.io/ocaml-decoders/decoders/decoders-sexplib/")
 (depends
  (ocaml (>= 4.03.0))
  (dune (>= 1.0))
  decoders
  sexplib0
  sexplib
  (odoc :with-doc)
  (containers :with-test)
  (ounit :with-test)))

(package
 (name decoders-yojson)
 (synopsis "Yojson backend for decoders")
 (description
   "A combinator library for \"decoding\" JSON-like values into your own Ocaml types, inspired by Elm's `Json.Decode` and `Json.Encode`.")
 (documentation "https://mattjbray.github.io/ocaml-decoders/decoders/decoders-yojson/")
 (depends
  (ocaml (>= 4.03.0))
  (dune (>= 1.0))
  decoders
  yojson
  (odoc :with-doc)
  (containers :with-test)
  (ounit :with-test)))
