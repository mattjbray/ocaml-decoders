open OUnit2

let sexplib_suite =
  let open Decoders_sexplib.Decode in

  let decoder_test ~decoder ~input ~expected _test_ctxt =
    match decode_string decoder input with
    | Ok value -> assert_equal value expected
    | Error error -> assert_string (Format.asprintf "%a" pp_error error)
  in

  "Sexplib" >:::
  [ "list string" >::
    decoder_test
      ~decoder:(list string)
      ~input:"(hello world)"
      ~expected:["hello"; "world"]
  ; "field_opt present" >::
    decoder_test
      ~decoder:(field_opt "optional" string)
      ~input:"((optional hello))"
      ~expected:(Some "hello")
  ; "field_opt missing" >::
    decoder_test
      ~decoder:(field_opt "optional" string)
      ~input:"()"
      ~expected:None
  ; "uncons" >::
    decoder_test
      ~decoder:(string |> uncons (function
          | "library" -> field "name" string
          | _ -> fail "Expected 'library'"
        ))
      ~input:"(library (name decoders))"
      ~expected:"decoders"
  ]

let () =
  "decoders" >:::
  [ sexplib_suite
  ]
  |> run_test_tt_main
