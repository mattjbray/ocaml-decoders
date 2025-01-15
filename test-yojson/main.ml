open OUnit2

type tree =
  | Leaf of int
  | Node of tree * tree

let yojson_basic_suite =
  let open Decoders_yojson.Basic.Decode in
  let decoder_test ~decoder ~input ~expected ?printer _test_ctxt =
    match decode_string decoder input with
    | Ok value ->
        assert_equal value expected ?printer
    | Error error ->
        assert_string (Format.asprintf "%a" pp_error error)
  in

  let list_string_test =
    "list string"
    >:: decoder_test
          ~decoder:(list string)
          ~input:"[\"Hello world\"]"
          ~expected:[ "Hello world" ]
  in

  let array_string_test =
    "array string"
    >:: decoder_test
          ~decoder:(array string)
          ~input:"[\"Hello world\"]"
          ~expected:[| "Hello world" |]
  in

  let fix_one_of_test =
    "fix one_of"
    >:: fun _ ->
    let tree_decoder =
      fix (fun tree_decoder ->
          let leaf_decoder = int |> map (fun i -> Leaf i) in
          let node_decoder =
            Pipeline.(
              decode (fun left right -> Node (left, right))
              |> required "left" tree_decoder
              |> required "right" tree_decoder)
          in
          one_of [ ("leaf", leaf_decoder); ("node", node_decoder) ] )
    in
    decoder_test
      ~decoder:tree_decoder
      ~input:"{\"left\":1, \"right\":{\"left\":2,\"right\":3}}"
      ~expected:(Node (Leaf 1, Node (Leaf 2, Leaf 3)))
      ()
  in

  let mut_rec_test =
    "mutual recursion"
    >:: fun _ ->
    let module M = struct
      type t1 =
        | T1_end
        | T1_more of t2

      and t2 =
        | T2_end
        | T2_more of t1

      let rec t1_to_string = function
        | T1_end ->
            "T1_end"
        | T1_more t2 ->
            Printf.sprintf "(T1_more %s)" (t2_to_string t2)


      and t2_to_string = function
        | T2_end ->
            "T2_end"
        | T2_more t1 ->
            Printf.sprintf "(T2_more %s)" (t1_to_string t1)
    end in
    let open M in
    let t1_decoder =
      fix (fun t1_decoder ->
          let t2 =
            nullable (field "t1" t1_decoder)
            |> map (function None -> T2_end | Some t1 -> T2_more t1)
          in
          let t1 =
            nullable (field "t2" t2)
            |> map (function None -> T1_end | Some t2 -> T1_more t2)
          in
          t1 )
    in
    decoder_test
      ()
      ~decoder:t1_decoder
      ~input:{|
          { "t2": { "t1": { "t2": null } } }
         |}
      ~expected:(T1_more (T2_more (T1_more T2_end)))
      ~printer:t1_to_string
  in

  let string_or_floatlit_test =
    "string or floatlit"
    >:: fun _ ->
    let empty_string =
      string
      |> and_then (function
             | "" ->
                 succeed ()
             | _ ->
                 fail "Expected an empty string" )
    in
    decoder_test
      ~decoder:(one_of [ ("empty", empty_string |> map (fun () -> None)) ])
      ~input:"\"\""
      ~expected:None
      ()
  in

  let grouping_errors_test =
    "grouping errors"
    >:: fun _test_ctxt ->
    let decoder =
      Pipeline.(
        decode (fun x y z -> (x, y, z))
        |> required
             "records"
             (list
                ( decode (fun x y z -> (x, y, z))
                |> required "x" (list string)
                |> required "y" int
                |> required "z" bool ) )
        |> required "hello" int
        |> required "another" int)
    in
    let input =
      {|
        {"records": [true, {"x": [1, "c", 3], "y": "hello"}], "hello": "world", "another": "error"}
      |}
    in
    let expected_error =
      let open Decoders in
      Error.tag
        {|in field "records"|}
        (Error.tag_group
           "while decoding a list"
           [ Error.tag
               "element 0"
               (Error.make
                  {|Expected an object with an attribute "x"|}
                  ~context:(`Bool true) )
           ; Error.tag
               "element 1"
               (Error.tag
                  {|in field "x"|}
                  (Error.tag_group
                     "while decoding a list"
                     [ Error.tag
                         "element 0"
                         (Error.make "Expected a string" ~context:(`Int 1))
                     ; Error.tag
                         "element 2"
                         (Error.make "Expected a string" ~context:(`Int 3))
                     ] ) )
           ] )
    in
    match decode_string decoder input with
    | Ok _ ->
        assert_string "Expected an error"
    | Error error ->
        assert_equal expected_error error ~printer:(fun e ->
            Format.asprintf "@,@[%a@]" pp_error e )
  in

  let tupleN_test =
    "TupleN"
    >:: fun _ ->
    let module M = struct
      type t2 = int * string

      let t2_to_string ((i, s) : t2) = Printf.sprintf "(%i, %s)" i s
    end in
    let open M in
    decoder_test
      ()
      ~decoder:(tuple2 int string)
      ~input:{|[149, "my string"]|}
      ~expected:(149, "my string")
      ~printer:t2_to_string
  in
  let empty_list_test =
    "empty_list"
    >:: fun _ ->
    decoder_test
      ()
      ~decoder:empty_list
      ~input:{|[]|}
      ~expected:()
      ~printer:(fun () -> Printf.sprintf "()")
  in
  let empty_list_nonempty_test =
    "empty_list_enforced"
    >:: fun _ ->
    let input = {|[1, 2]|} in
    let expected_error =
      let open Decoders in
      Error.make ~context:(`List [ `Int 1; `Int 2 ]) "Expected an empty list"
    in
    match decode_string empty_list input with
    | Ok _ ->
        assert_string "Expected an error"
    | Error error ->
        assert_equal expected_error error ~printer:(fun e ->
            Format.asprintf "@,@[%a@]" pp_error e )
  in

  "Yojson.Basic"
  >::: [ list_string_test
       ; array_string_test
       ; fix_one_of_test
       ; mut_rec_test
       ; string_or_floatlit_test
       ; grouping_errors_test
       ; tupleN_test
       ; empty_list_test
       ; empty_list_nonempty_test
       ]


let yojson_raw_suite =
  let open Decoders_yojson.Raw.Decode in
  let decoder_test ~decoder ~input ~expected _test_ctxt =
    match decode_string decoder input with
    | Ok value ->
        assert_equal value expected
    | Error error ->
        assert_string (Format.asprintf "%a" pp_error error)
  in

  "Yojson.Raw"
  >::: [ "list string"
         >:: decoder_test
               ~decoder:(list string)
               ~input:"[\"Hello world\"]"
               ~expected:[ "Hello world" ]
       ; ( "fix one_of"
         >:: fun _ ->
         let tree_decoder =
           fix (fun tree_decoder ->
               let leaf_decoder = int |> map (fun i -> Leaf i) in
               let node_decoder =
                 Pipeline.(
                   decode (fun left right -> Node (left, right))
                   |> required "left" tree_decoder
                   |> required "right" tree_decoder)
               in
               one_of [ ("leaf", leaf_decoder); ("node", node_decoder) ] )
         in
         decoder_test
           ~decoder:tree_decoder
           ~input:"{\"left\":1, \"right\":{\"left\":2,\"right\":3}}"
           ~expected:(Node (Leaf 1, Node (Leaf 2, Leaf 3)))
           () )
       ; ( "string or floatlit"
         >::
         let empty_string =
           string
           |> and_then (function
                  | "" ->
                      succeed None
                  | _ ->
                      fail "Expected an empty string" )
         in
         decoder_test
           ~decoder:
             (list
                (one_of
                   [ ("empty", empty_string)
                   ; ("floatlit", floatlit |> map (fun x -> Some x))
                   ] ) )
           ~input:{|["", 123, 123.45]|}
           ~expected:[ None; Some "123"; Some "123.45" ] )
       ]


let () =
  "decoders" >::: [ yojson_basic_suite; yojson_raw_suite ] |> run_test_tt_main
