(lang dune 3.1)
(name decoders)
(generate_opam_files true)
(license ISC)
(authors "Matt Bray <mattjbray@gmail.com>")
(maintainers "Matt Bray <mattjbray@gmail.com>")
(source (github "mattjbray/ocaml-decoders"))
(documentation "https://mattjbray.github.io/ocaml-decoders/")

(package
 (name decoders-bencode)
 (authors "Simon Cruanes <simon@imandra.ai>")
 (synopsis "Bencode backend for decoders")
 (description
   "A combinator library for \"decoding\" JSON-like values into your own Ocaml types, inspired by Elm's `Json.Decode` and `Json.Encode`.")
 (depends
  (ocaml (>= 4.03.0))
  (decoders (= :version))
  (bencode (>= 2.0))
  (odoc :with-doc)
  (containers (and :with-test (>= 0.16)))
  (ounit2 :with-test)))

(package
 (name decoders-cbor)
 (synopsis "CBOR backend for decoders")
 (description
   "A combinator library for \"decoding\" JSON-like values into your own Ocaml types, inspired by Elm's `Json.Decode` and `Json.Encode`.")
 (depends
  (ocaml (>= 4.03.0))
  (decoders (= :version))
  cbor
  (odoc :with-doc)
  (containers (and :with-test (>= 0.16)))
  (ounit2 :with-test)))

(package
 (name decoders-ezjsonm)
 (synopsis "Ezjsonm backend for decoders")
 (description
   "A combinator library for \"decoding\" JSON-like values into your own Ocaml types, inspired by Elm's `Json.Decode` and `Json.Encode`.")
 (depends
  (ocaml (>= 4.03.0))
  (decoders (= :version))
  (ezjsonm (>= 0.4.0))
  (odoc :with-doc)
  (containers (and :with-test (>= 0.16)))
  (ounit2 :with-test)))

(package
 (name decoders-jsonaf)
 (synopsis "Jsonaf backend for decoders")
 (authors "Vlad Dumitru <vlad@dumitru.net>" "Matt Bray <mattjbray@gmail.com>")
 (description
   "A combinator library for \"decoding\" JSON-like values into your own Ocaml types, inspired by Elm's `Json.Decode` and `Json.Encode`.")
 (depends
  (ocaml (>= 4.10.0))
  (decoders (= :version))
  (jsonaf (>= 0.15.0))
  (odoc :with-doc)
  (ounit2 :with-test)))

(package
 (name decoders-jsonm)
 (synopsis "Jsonm backend for decoders")
 (description
   "A combinator library for \"decoding\" JSON-like values into your own Ocaml types, inspired by Elm's `Json.Decode` and `Json.Encode`.")
 (depends
  (ocaml (>= 4.03.0))
  (decoders (= :version))
  jsonm
  (odoc :with-doc)
  (containers (and :with-test (>= 0.16)))
  (ounit2 :with-test)))

(package
 (name decoders-msgpck)
 (authors "Matt Bray <mattjbray@gmail.com>" "Simon Cruanes <simon@imandra.ai>")
 (synopsis "Msgpck backend for decoders")
 (description
   "A combinator library for \"decoding\" JSON-like values into your own Ocaml types, inspired by Elm's `Json.Decode` and `Json.Encode`.")
 (depends
  (ocaml (>= 4.03.0))
  (decoders (= :version))
  (msgpck (>= 1.3))
  (ocplib-endian (>= 0.6)) ; lower bound missing from msgpck
  (odoc :with-doc)
  (containers (and :with-test (>= 0.16)))
  (ounit2 :with-test)))

(package
 (name decoders-sexplib)
 (synopsis "Sexplib backend for decoders")
 (description
   "A combinator library for \"decoding\" JSON-like values into your own Ocaml types, inspired by Elm's `Json.Decode` and `Json.Encode`.")
 (depends
  (ocaml (>= 4.03.0))
  (decoders (= :version))
  sexplib0
  sexplib
  (odoc :with-doc)
  (containers (and :with-test (>= 0.16)))
  (ounit2 :with-test)))

(package
 (name decoders-ezxmlm)
 (synopsis "Ezxmlm backend for decoders")
 (description
   "A combinator library for \"decoding\" JSON-like values into your own Ocaml types, inspired by Elm's `Json.Decode` and `Json.Encode`.")
 (depends
  (ocaml (>= 4.03.0))
  (decoders (= :version))
  (ezxmlm (>= 1.1.0))
  (containers (and :with-test (>= 0.16)))
  (odoc :with-doc)))

(package
 (name decoders-otoml)
 (synopsis "Otoml backend for decoders")
 (depends
  (ocaml (>= 4.03.0))
  (decoders (= :version))
  (otoml (>= 1.0))
  (containers (and :with-test (>= 0.16)))
  (odoc :with-doc)))

(package
 (name decoders-yojson)
 (synopsis "Yojson backend for decoders")
 (description
   "A combinator library for \"decoding\" JSON-like values into your own Ocaml types, inspired by Elm's `Json.Decode` and `Json.Encode`.")
 (depends
  (ocaml (>= 4.03.0))
  (decoders (= :version))
  (yojson (>= 1.6.0))
  (odoc :with-doc)
  (containers (and :with-test (>= 0.16)))
  (ounit2 :with-test)))

(package
 (name decoders)
 (synopsis "Elm-inspired decoders for Ocaml")
 (description
   "A combinator library for \"decoding\" JSON-like values into your own Ocaml types, inspired by Elm's `Json.Decode` and `Json.Encode`.")
 (depends
  (ocaml (>= 4.03.0))
  (odoc :with-doc)
  (containers (and :with-test (>= 0.16)))))
