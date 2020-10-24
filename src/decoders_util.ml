(** Util module used for native builds (excluded in bs-config.json) *)
module My_result = struct
  type ('good, 'bad) t = ('good, 'bad) result =
    | Ok of 'good
    | Error of 'bad

  let return x = Ok x

  let map f e = match e with Ok x -> Ok (f x) | Error s -> Error s

  let map_err f e = match e with Ok _ as res -> res | Error y -> Error (f y)

  let flat_map f e = match e with Ok x -> f x | Error s -> Error s

  module Infix = struct
    let ( >|= ) e f = map f e

    let ( >>= ) e f = flat_map f e
  end
end

module My_opt = struct
  let return x = Some x

  let map f = function None -> None | Some x -> Some (f x)

  let flat_map f o = match o with None -> None | Some x -> f x
end

module My_list = struct
  let take n l =
    let rec direct i n l =
      match l with
      | [] ->
          []
      | _ when i = 0 ->
          safe n [] l
      | x :: l' ->
          if n > 0 then x :: direct (i - 1) (n - 1) l' else []
    and safe n acc l =
      match l with
      | [] ->
          List.rev acc
      | _ when n = 0 ->
          List.rev acc
      | x :: l' ->
          safe (n - 1) (x :: acc) l'
    in
    direct 500 n l


  let map f l =
    let rec direct f i l =
      match l with
      | [] ->
          []
      | [ x ] ->
          [ f x ]
      | [ x1; x2 ] ->
          let y1 = f x1 in
          [ y1; f x2 ]
      | [ x1; x2; x3 ] ->
          let y1 = f x1 in
          let y2 = f x2 in
          [ y1; y2; f x3 ]
      | _ when i = 0 ->
          List.rev (List.rev_map f l)
      | x1 :: x2 :: x3 :: x4 :: l' ->
          let y1 = f x1 in
          let y2 = f x2 in
          let y3 = f x3 in
          let y4 = f x4 in
          y1 :: y2 :: y3 :: y4 :: direct f (i - 1) l'
    in
    direct f 500 l


  let all_some l =
    try Some (map (function Some x -> x | None -> raise Exit) l) with
    | Exit ->
        None


  let mapi f l =
    let r = ref 0 in
    map
      (fun x ->
        let y = f !r x in
        incr r ;
        y)
      l


  let find_map f l =
    let rec aux f = function
      | [] ->
          None
      | x :: l' ->
        (match f x with Some _ as res -> res | None -> aux f l')
    in
    aux f l


  let filter_map f l =
    let rec recurse acc l =
      match l with
      | [] ->
          List.rev acc
      | x :: l' ->
          let acc' = match f x with None -> acc | Some y -> y :: acc in
          recurse acc' l'
    in
    recurse [] l


  let fold_left = List.fold_left

  let direct_depth_append_ = 10_000

  let append l1 l2 =
    let rec direct i l1 l2 =
      match l1 with
      | [] ->
          l2
      | _ when i = 0 ->
          safe l1 l2
      | x :: l1' ->
          x :: direct (i - 1) l1' l2
    and safe l1 l2 = List.rev_append (List.rev l1) l2 in
    match l1 with
    | [] ->
        l2
    | [ x ] ->
        x :: l2
    | [ x; y ] ->
        x :: y :: l2
    | _ ->
        direct direct_depth_append_ l1 l2


  let ( @ ) = append

  let flat_map f l =
    let rec aux f l kont =
      match l with
      | [] ->
          kont []
      | x :: l' ->
          let y = f x in
          let kont' tail =
            match y with
            | [] ->
                kont tail
            | [ x ] ->
                kont (x :: tail)
            | [ x; y ] ->
                kont (x :: y :: tail)
            | l ->
                kont (append l tail)
          in
          aux f l' kont'
    in
    aux f l (fun l -> l)
end

let with_file_in file f =
  let ic = open_in file in
  try
    let res = f ic in
    close_in ic ;
    res
  with
  | e ->
      close_in_noerr ic ;
      raise e


let read_all ic : string =
  let buf = ref (Bytes.create 2048) in
  let len = ref 0 in
  try
    while true do
      (* resize *)
      if !len = Bytes.length !buf then buf := Bytes.extend !buf 0 !len ;
      assert (Bytes.length !buf > !len) ;
      let n = input ic !buf !len (Bytes.length !buf - !len) in
      len := !len + n ;
      if n = 0 then raise Exit
      (* exhausted *)
    done ;
    assert false (* never reached*)
  with
  | Exit ->
      Bytes.sub_string !buf 0 !len
