(* Note: also copied to src-bs/shims_let_ops_.ml *)
let shims_let_op_pre_408 =
  {|
  module type S = sig type ('i, 'o, 'e) t end
  module Make(X:sig type ('i, 'o, 'e) t end)
    : S with type ('i, 'o, 'e) t = ('i, 'o, 'e) X.t  = struct
    type ('i, 'o, 'e) t = ('i, 'o, 'e) X.t
  end
|}


let shims_let_op_post_408 =
  {|
  module type S = sig
    type ('i, 'o, 'e) t
    val ( let+ ) : ('i, 'a, 'e) t -> ('a -> 'b) -> ('i, 'b, 'e) t
    val ( and+ ) : ('i, 'a, 'e) t -> ('i, 'b, 'e) t -> ('i, 'a * 'b, 'e) t
    val ( let* ) : ('i, 'a, 'e) t -> ('a -> ('i, 'b, 'e) t) -> ('i, 'b, 'e) t
    val ( and* ) : ('i, 'a, 'e) t -> ('i, 'b, 'e) t -> ('i, 'a * 'b, 'e) t
  end

  module Make(X:sig
    type ('i, 'a, 'e) t
    val (>|=) : ('i, 'a, 'e) t -> ('a -> 'b) -> ('i, 'b, 'e) t
    val monoid_product : ('i, 'a, 'e) t -> ('i, 'b, 'e) t -> ('i, ('a * 'b), 'e) t
    val (>>=) : ('i, 'a, 'e) t -> ('a -> ('i, 'b, 'e) t) -> ('i, 'b, 'e) t
  end) : S with type ('i, 'o, 'e) t = ('i, 'o, 'e) X.t = struct
    type ('i, 'o, 'e) t = ('i, 'o, 'e) X.t
    let (let+) = X.(>|=)
    let (and+) = X.monoid_product
    let (let*) = X.(>>=)
    let (and*) = X.monoid_product
  end[@@inline]
|}


let () =
  let version = Sys.ocaml_version in
  let major, minor = Scanf.sscanf version "%u.%u" (fun maj min -> (maj, min)) in
  print_endline
    ( if (major, minor) >= (4, 8)
    then shims_let_op_post_408
    else shims_let_op_pre_408 ) ;
  ()
