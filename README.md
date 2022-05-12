# ocaml-decoders: Elm-inspired decoders for OCaml

A combinator library for "decoding" JSON-like values into your own OCaml types, inspired by Elm's `Json.Decode` and `Json.Encode`.

> Eh?

An OCaml program having a JSON (or YAML) data source usually goes something like this:

1. Get your data from somewhere. Now you have a `string`.
2. *Parse* the `string` as JSON (or YAML). Now you have a `Yojson.Basic.t`, or maybe an `Ezjsonm.value`.
3. *Decode* the JSON value to an OCaml type that's actually useful for your program's domain.

This library helps with step 3.

# Getting started

Install one of the supported decoder backends:

### For ocaml

```
opam install decoders-bencode      # For bencode
opam install decoders-cbor         # For CBOR
opam install decoders-ezjsonm      # For ezjsonm
opam install decoders-jsonm        # For jsonm
opam install decoders-msgpck       # For msgpck
opam install decoders-sexplib      # For sexplib
opam install decoders-yojson       # For yojson
```

### For bucklescript

```
npm install --save-dev bs-decoders
```

## Decoding

Now we can start decoding stuff!

First, a module alias to save some keystrokes. In this guide, we'll parse JSON
using `Yojson`'s `Basic` variant.

```ocaml
utop # module D = Decoders_yojson.Basic.Decode;;
module D = Decoders_yojson.Basic.Decode
```

Let's set our sights high and decode an integer.

```ocaml
utop # D.decode_value D.int (`Int 1);;
- : (int, error) result = Ok 1
```

Nice! We used `decode_value`, which takes a `decoder` and a `value` (in this
case a `Yojson.Basic.t`) and... decodes the value.

```ocaml
utop # D.decode_value;;
- : 'a decoder -> value -> ('a, error) result = <fun>
```

For convenience we also have `decode_string`, which takes a `string` and calls
`Yojson`'s parser under the hood.

```ocaml
utop # D.decode_string D.int "1";;
- : (int, error) result = Ok 1
```

What about a `list` of `int`s? Here's where the "combinator" part comes in.

```ocaml
utop # D.decode_string D.(list int) "[1,2,3]";;
- : (int list, error) result = Ok [1; 2; 3]
```

Success!

Ok, so what if we get some unexpected JSON?

```ocaml
utop # #install_printer D.pp_error;;
utop # D.decode_string D.(list int) "[1,2,true]";;
- : (int list, error) result =
Error while decoding a list: element 2: Expected an int, but got true
```

## Complicated JSON structure

To decode a JSON object with many fields, we can use the let-binding operators
(`let*`, etc.) from the `Infix` module.

```ocaml
type my_user =
  { name : string
  ; age : int
  }

let my_user_decoder : my_user decoder =
  let open D in
  let* name = field "name" string in
  let* age = field "age" int in
  succeed { name; age }
```

> *Note for Bucklescript users*: let-binding operators are not currently available
> in Bucklescript, so if you need your decoders to be compatible with Bucklescript
> you can use the monadic bind operator (`>>=`):
>
> ```ocaml
> let my_user_decoder : my_user decoder =
>   let open D in
>   field "name" string >>= fun name ->
>   field "age" int >>= fun age ->
>   succeed { name; age }
> ```

We can also use these operators to decode objects with inconsistent structure. Say, for
example, our JSON is a list of shapes. Squares have a side length, circles have
a radius, and triangles have a base and a height.

```json
[{ "shape": "square", "side": 11 },
 { "shape": "circle", "radius": 5 },
 { "shape": "triange", "base": 3, "height": 7 }]
```

We could represent these types in OCaml and decode them like this:

```ocaml
type shape =
  | Square of int
  | Circle of int
  | Triangle of int * int

let square_decoder : shape decoder =
  D.(let+ s = field "side" int in Square s)

let circle_decoder : shape decoder =
  D.(let+ r = field "radius" int in Circle r)

let triangle_decoder : shape decoder =
  D.(
    let* b = field "base" int in
    let+ h = field "height" int in
    Triangle (b, h)
  )

let shape_decoder : shape decoder =
  let open D in
  let* shape = field "shape" string in
  match shape with
  | "square" -> square_decoder
  | "circle" -> circle_decoder
  | "triangle" -> triangle_decoder
  | _ -> fail "Expected a shape"


let decode_list (json_string : string) : (shape list, _) result =
  D.(decode_string (list shape_decoder) json_string)
```

Now, say that we didn't have the benefit of the `"shape"` field describing the
type of the shape in our JSON list. We can still decode the shapes by trying
each decoder in turn using the `one_of` combinator.

`one_of` takes a list of `string * 'a decoder` pairs and tries each decoder in
turn. The `string` element of each pair is just used to name the decoder in
error messages.

```ocaml
let shape_decoder_2 : shape decoder =
  D.(
    one_of
      [ ("a square", square_decoder)
      ; ("a circle", circle_decoder)
      ; ("a triangle", triangle_decoder)
      ]
  )
```

## Generic decoders


Suppose our program deals with users and roles. We want to decode our JSON input
into these types.

```ocaml
type role = Admin | User

type user =
  { name : string
  ; roles : role list
  }
```

Let's define our decoders. We'll write a module functor so we can re-use the
same decoders across different JSON libraries, with YAML input, or with
Bucklescript.

```ocaml
module My_decoders(D : Decoders.Decode.S) = struct
  open D

  let role : role decoder =
    string >>= function
    | "ADMIN" -> succeed Admin
    | "USER" -> succeed User
    | _ -> fail "Expected a role"

  let user : user decoder =
    let* name = field "name" string in
    let* roles = field "roles" (list role) in
    succeed { name; roles }
end

module My_yojson_decoders = My_decoders(Decoders_yojson.Basic.Decode)
```

Great! Let's try them out.

```ocaml
utop # open My_yojson_decoders;;
utop # D.decode_string role {| "USER" |};;
- : (role, error) result = Ok User

utop # D.decode_string D.(field "users" (list user))
         {| {"users": [{"name": "Alice", "roles": ["ADMIN", "USER"]},
                       {"name": "Bob", "roles": ["USER"]}]}
          |};;
- : (user list, error) result =
Ok [{name = "Alice"; roles = [Admin; User]}; {name = "Bob"; roles = [User]}]
```

Let's introduce an error in the JSON:

```ocaml
utop # D.decode_string D.(field "users" (list user))
         {| {"users": [{"name": "Alice", "roles": ["ADMIN", "USER"]},
                       {"name": "Bob", "roles": ["SUPER_USER"]}]}
          |};;
- : (user list, error) result =
Error
 in field "users":
   while decoding a list:
     element 1:
       in field "roles":
         while decoding a list:
           element 0: Expected a role, but got "SUPER_USER"
```

We get a nice pointer that we forgot to handle the `SUPER_USER` role.

## Encoding

`ocaml-decoders` also has support for defining backend-agnostic encoders, for
turning your OCaml values into JSON values.

```ocaml
module My_encoders(E : Decoders.Encode.S) = struct
  open E

  let role : role encoder =
    function
    | Admin -> string "ADMIN"
    | User -> string "USER"

  let user : user encoder =
    fun u ->
      obj
        [ ("name", string u.name)
        ; ("roles", list role u.roles)
        ]
end

module My_yojson_encoders = My_encoders(Decoders_yojson.Basic.Encode)
```

```ocaml
utop # module E = Decoders_yojson.Basic.Encode;;
utop # open My_yojson_encoders;;
utop # let users =
  [ {name = "Alice"; roles = [Admin; User]}
  ; {name = "Bob"; roles = [User]}
  ];;
utop # E.encode_string E.obj [("users", E.list user users)];;
- : string =
"{\"users\":[{\"name\":\"Alice\",\"roles\":[\"ADMIN\",\"USER\"]},{\"name\":\"Bob\",\"roles\":[\"USER\"]}]}"
```

## API Documentation

For more details, see the API documentation:

* [`Decoders.Decode.S`](https://mattjbray.github.io/ocaml-decoders/decoders/Decoders/Decode/module-type-S/index.html) interface
* [`Decoders.Encode.S`](https://mattjbray.github.io/ocaml-decoders/decoders/Decoders/Encode/module-type-S/index.html) interface

# Release

After updating CHANGES.md:

```
npm version <newversion> # e.g. npm version 0.7.0
git push --tags
dune-release
npm publish
```
