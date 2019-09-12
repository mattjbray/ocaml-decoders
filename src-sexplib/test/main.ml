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
  ; "list string (one element)" >::
    decoder_test
      ~decoder:(list string)
      ~input:"(hello)"
      ~expected:["hello"]
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
    let (>>=::) head tail = uncons tail head in
    decoder_test
      ~input:"(library \
              (name decoders-sexplib) \
              (libraries decoders sexplib0))"
      ~decoder:(string >>=:: function
        | "library" ->
          field "name" string >>= fun name ->
          field "libraries"
            (one_of
               [ ("list", list string)
               ; ("string", string >|= fun s -> [s])
               ])
          >>= fun libs ->
          succeed (name, libs)
        | _ -> fail "Expected 'library'")
      ~expected:("decoders-sexplib", [ "decoders"; "sexplib0" ])
  ]

let () =
  "decoders" >:::
  [ sexplib_suite
  ]
  |> run_test_tt_main
