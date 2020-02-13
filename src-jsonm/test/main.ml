open OUnit2

let jsonm_suite =
  let open Decoders_jsonm.Encode in

  let test _test_ctxt =
    let b = Buffer.create 0 in
    let env =
      make_env ~encoder:(Jsonm.encoder ~minify:true (`Buffer b)) ()
    in
    let run e = e env in
    run (array_start >> int 0 >> int 1 >> string "hello" >> array_end >> end_ );
    assert_equal ~printer:CCFun.id {|[0,1,"hello"]|}  (Buffer.contents b)
  in

  "Jsonm" >:::
  [ "encoding a list" >:: test
  ]

let () =
  "encoders" >:::
  [ jsonm_suite
  ]
  |> run_test_tt_main
