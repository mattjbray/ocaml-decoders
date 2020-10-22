(* Note: copied from src/gen/mkshims.ml *)
module type S = sig type 'a t_let end
module Make(X:sig type 'a t end) = struct type 'a t_let = 'a X.t end

module type S2 = sig type ('a,'b) t_let2 end
module Make2(X:sig type ('a,'b) t end) = struct type ('a,'b) t_let2 = ('a,'b) X.t end
