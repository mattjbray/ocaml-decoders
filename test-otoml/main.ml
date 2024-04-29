open OUnit2

let otoml_suite =
  let open Decoders_otoml.Decode in
  let decoder_test ~decoder ~input ~expected _test_ctxt =
    match decode_string decoder input with
    | Ok value ->
        assert_equal value expected
    | Error error ->
        assert_string (Format.asprintf "%a" pp_error error)
  in

  "otoml"
  >::: [ "basic"
         >:: decoder_test
               ~decoder:
                 (let+ a = field "a" int
                  and+ b = field "b" float in
                  (a, b) )
               ~input:"a=1\nb=42.5"
               ~expected:(1, 42.5)
       ; "table"
         >:: decoder_test
               ~decoder:(field "sub" (field_opt "a" int))
               ~input:"[sub]\na = 42"
               ~expected:(Some 42)
       ; "table (field absent)"
         >:: decoder_test
               ~decoder:(field "sub" (field_opt "a" int))
               ~input:"[sub]\nb = false"
               ~expected:None
       ]


let () = "decoders" >::: [ otoml_suite ] |> run_test_tt_main
