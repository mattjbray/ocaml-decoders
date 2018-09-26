open OUnit2

type tree = Leaf of int | Node of tree * tree

let yojson_basic_suite =
  let open Decoders_yojson.Basic.Decode in

  let decoder_test ~decoder ~input ~expected _test_ctxt =
    match decode_string decoder input with
    | Ok value -> assert_equal value expected
    | Error error -> assert_string (Format.asprintf "%a" pp_error error)
  in

  let list_string_test =
    "list string" >::
    decoder_test
      ~decoder:(list string)
      ~input:"[\"Hello world\"]"
      ~expected:["Hello world"]
  in

  let fix_one_of_test =
    "fix one_of" >::
    (fun _ ->
       let tree_decoder =
         fix
           (fun tree_decoder ->
              let leaf_decoder =
                int |> map (fun i -> Leaf i)
              in
              let node_decoder =
                Pipeline.(
                  decode (fun left right -> Node (left, right))
                  |> required "left" tree_decoder
                  |> required "right" tree_decoder)
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
  in

  let string_or_floatlit_test =
    "string or floatlit" >::
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
  in

  let grouping_errors_test =
    "grouping errors" >:: fun _test_ctxt ->
      let decoder =
        Pipeline.(
          decode (fun x y z -> (x, y, z))
          |> required "records"
            (list
               (decode (fun x y z -> (x, y, z))
                |> required "x" (list string)
                |> required "y" int
                |> required "z" bool
               ))
          |> required "hello" int
          |> required "another" int)
      in
      let input = {|
        {"records": [true, {"x": [1, "c", 3], "y": "hello"}], "hello": "world", "another": "error"}
      |}
      in
      let expected_error =
        let open Decoders.Decode in
        Decoder_errors
          [ Decoder_tag
              ( {|in field "records"|}
              , Decoder_tag
                  ( "while decoding a list"
                  , Decoder_errors
                      [ Decoder_tag
                          ( "element 0"
                          , Decoder_errors
                              [ Decoder_error ({|Expected an object with an attribute "x"|}, Some (`Bool true))
                              ; Decoder_error ({|Expected an object with an attribute "y"|}, Some (`Bool true))
                              ; Decoder_error ({|Expected an object with an attribute "z"|}, Some (`Bool true))
                              ])
                      ; Decoder_tag
                          ( "element 1"
                          , Decoder_errors
                              [ Decoder_tag
                                  ( {|in field "x"|}
                                  , Decoder_tag
                                      ( "while decoding a list"
                                      , Decoder_errors
                                          [ Decoder_tag
                                              ( "element 0"
                                              , Decoder_error ("Expected a string", Some (`Int 1))
                                              )
                                          ; Decoder_tag
                                              ( "element 2"
                                              , Decoder_error ("Expected a string", Some (`Int 3))
                                              )
                                          ]
                                      )
                                  )
                              ; Decoder_tag
                                  ( {|in field "y"|}
                                  , Decoder_error ("Expected an int", Some (`String "hello"))
                                  )
                              ; Decoder_error
                                  ( {|Expected an object with an attribute "z"|}
                                  , Some
                                      (`Assoc
                                         [ ("x", `List [ `Int 1; `String "c"; `Int 3 ])
                                         ; ( "y", `String "hello" )
                                         ])
                                  )
                              ]
                          )
                      ]
                  )
              )
          ; Decoder_tag
              ( {|in field "hello"|}
              , Decoder_error ("Expected an int", Some (`String "world"))
              )
          ; Decoder_tag
              ( {|in field "another"|}
              , Decoder_error ("Expected an int", Some (`String "error"))
              )
          ]
      in
      match decode_string decoder input with
      | Ok _ -> assert_string "Expected an error"
      | Error error ->
        assert_equal expected_error error
          ~printer:(fun e -> Format.asprintf "@,@[%a@]" pp_error e)
  in

  "Yojson.Basic" >:::
  [ list_string_test
  ; fix_one_of_test
  ; string_or_floatlit_test
  ; grouping_errors_test
  ]

let yojson_raw_suite =
  let open Decoders_yojson.Raw.Decode in

  let decoder_test ~decoder ~input ~expected _test_ctxt =
    match decode_string decoder input with
    | Ok value -> assert_equal value expected
    | Error error -> assert_string (Format.asprintf "%a" pp_error error)
  in

  "Yojson.Raw" >:::
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
                Pipeline.(
                  decode (fun left right -> Node (left, right))
                  |> required "left" tree_decoder
                  |> required "right" tree_decoder)
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
          | "" -> succeed None
          | _ -> fail "Expected an empty string")
    in
    decoder_test
      ~decoder:(
        list
          (one_of
             [ ("empty", empty_string)
             ; ("floatlit", floatlit |> map (fun x -> Some x))
             ])
      )
      ~input:{|["", 123, 123.45]|}
      ~expected:[ None; Some "123"; Some "123.45" ]
  ]

let ezjsonm_suite =
  let open Decoders_ezjsonm.Decode in

  let decoder_test ~decoder ~input ~expected _test_ctxt =
    match decode_string decoder input with
    | Ok value -> assert_equal value expected
    | Error error -> assert_string (Format.asprintf "%a" pp_error error)
  in

  "Ezjsonm" >:::
  [ "list string" >::
    decoder_test
      ~decoder:(list string)
      ~input:{|["hello", "world"]|}
      ~expected:["hello"; "world"]
  ]

let ezjsonm_encoders_suite =
  let open Decoders_ezjsonm.Encode in
  "Ezjsonm encoders" >:::
  [ "list string" >::
    (fun _ctxt ->
       assert_equal ~printer:CCFun.id
         {|["hello","world"]|}
         (encode_string (list string) ["hello"; "world"])
    )
  ; "string" >::
    (fun _ctxt ->
       assert_equal ~printer:CCFun.id
         {|"hello"|}
         (encode_string string "hello")
    )
  ]

let () =
  "decoders" >:::
  [ yojson_basic_suite
  ; yojson_raw_suite
  ; ezjsonm_suite
  ; ezjsonm_encoders_suite
  ]
  |> run_test_tt_main
