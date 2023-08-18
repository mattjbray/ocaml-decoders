(* Note: copied from src/gen/mkshims.ml *)
module type I = sig
  type ('i, 'a) t

  val ( >|= ) : ('i, 'a) t -> ('a -> 'b) -> ('i, 'b) t

  val monoid_product : ('i, 'a) t -> ('i, 'b) t -> ('i, 'a * 'b) t

  val ( >>= ) : ('i, 'a) t -> ('a -> ('i, 'b) t) -> ('i, 'b) t
end

module type S = sig
  type ('i, 'o) t_let
  val ( let+ ) : ('i, 'a) t_let -> ('a -> 'b) -> ('i, 'b) t_let
  val ( and+ ) : ('i, 'a) t_let -> ('i, 'b) t_let -> ('i, 'a * 'b) t_let
  val ( let* ) : ('i, 'a) t_let -> ('a -> ('i, 'b) t_let) -> ('i, 'b) t_let
  val ( and* ) : ('i, 'a) t_let -> ('i, 'b) t_let -> ('i, 'a * 'b) t_let
end

module Make(X : I) : S with type ('i, 'o) t_let = ('i, 'o) X.t =
  struct
    type ('i, 'o) t_let = ('i, 'o) X.t
    let (let+) = X.(>|=)
    let (and+) = X.monoid_product
    let (let*) = X.(>>=)
    let (and*) = X.monoid_product
end[@@inline]
