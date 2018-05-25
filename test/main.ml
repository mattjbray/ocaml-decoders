open OUnit2

type tree = Leaf of int | Node of tree * tree

let yojson_suite =
  let open Decode_yojson in

  let decoder_test ~decoder ~input ~expected test_ctxt =
       match Decode_yojson.decode_string decoder input with
       | Ok value -> assert_equal value expected
       | Error error -> assert_string (Format.asprintf "%a" Decode_yojson.pp_error error)
  in

  "Yojson" >:::
  [ "list string" >::
    decoder_test
      ~decoder:(list string)
      ~input:"[\"Hello world\"]"
      ~expected:["Hello world"]
  ; "fix one_of" >::
    (fun _ ->
       let tree_decoder =
         fix
           (fun tree_decoder ->
              let leaf_decoder =
                int |> map (fun i -> Leaf i)
              in
              let node_decoder =
                decode (fun left right -> Node (left, right))
                |> required "left" tree_decoder
                |> required "right" tree_decoder
              in
              one_of
                [ ("leaf", leaf_decoder)
                ; ("node", node_decoder)
                ]
           )
       in
    decoder_test
      ~decoder:tree_decoder
      ~input:"{\"left\":1, \"right\":{\"left\":2,\"right\":3}}"
      ~expected:(
        Node (Leaf 1, Node (Leaf 2, Leaf 3))
      )
      ()

    )
  ; "string or floatlit" >::
    let empty_string =
      string |> and_then (function
      | "" -> succeed ()
      | _ -> fail "Expected an empty string")
    in
    decoder_test
      ~decoder:(
        one_of
          [ "empty", empty_string |> map (fun () -> None)
          ]
      )
      ~input:"\"\""
      ~expected:None
  ]

let () =
  "decoders" >:::
  [ yojson_suite
  ]
  |> run_test_tt_main
