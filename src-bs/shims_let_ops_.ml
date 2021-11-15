(* Note: copied from src/gen/mkshims.ml *)
module type S = sig
  type ('i, 'a, 'e) t
end

module Make (X : sig
  type ('i, 'a, 'e) t
end) =
struct
  type ('i, 'a, 'e) t = ('i, 'a, 'e) X.t
end
