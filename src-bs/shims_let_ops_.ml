(* Note: copied from src/gen/mkshims.ml *)
module type I = sig
  type ('i, 'a) t

  val ( >|= ) : ('i, 'a) t -> ('a -> 'b) -> ('i, 'b) t

  val monoid_product : ('i, 'a) t -> ('i, 'b) t -> ('i, 'a * 'b) t

  val ( >>= ) : ('i, 'a) t -> ('a -> ('i, 'b) t) -> ('i, 'b) t
end

module type S = sig
  type ('i, 'a) t_let
end

module Make (X : I) = struct
  type ('i, 'a) t_let = ('i, 'a) X.t
end
