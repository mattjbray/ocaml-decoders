(* Note: copied from src/gen/mkshims.ml *)
module type I = sig
  type ('i, 'a, 'e) t

  val ( >|= ) : ('i, 'a, 'e) t -> ('a -> 'b) -> ('i, 'b, 'e) t

  val monoid_product : ('i, 'a, 'e) t -> ('i, 'b, 'e) t -> ('i, 'a * 'b, 'e) t

  val ( >>= ) : ('i, 'a, 'e) t -> ('a -> ('i, 'b, 'e) t) -> ('i, 'b, 'e) t
end

module type S = sig
  type ('i, 'a, 'e) t_let
end

module Make (X : I) = struct
  type ('i, 'a, 'e) t_let = ('i, 'a, 'e) X.t
end
