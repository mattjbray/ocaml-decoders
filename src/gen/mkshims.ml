(* Note: also copied to src-bs/shims_let_ops_.ml *)
let shims_let_op_pre_408 =
  {|
   module type S = sig type 'a t_let end
   module Make(X:sig type 'a t end) = struct type 'a t_let = 'a X.t end
|}


let shims_let_op_post_408 =
  {|
    module type S = sig
      type 'a t_let
      val (let+) : 'a t_let -> ('a -> 'b) -> 'b t_let
      val (and+) : 'a t_let -> 'b t_let -> ('a * 'b) t_let
      val (let*) : 'a t_let -> ('a -> 'b t_let) -> 'b t_let
      val (and*) : 'a t_let -> 'b t_let -> ('a * 'b) t_let
    end
   module Make(X:sig
    type 'a t
    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
    val monoid_product : 'a t -> 'b t -> ('a * 'b) t
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    end) : S with type 'a t_let = 'a X.t = struct
      type 'a t_let = 'a X.t
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
