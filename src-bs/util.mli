module My_result : sig
  type ('good, 'bad) t = ('good, 'bad) Belt.Result.t =
    | Ok of 'good
    | Error of 'bad

  val return : 'good -> ('good, 'bad) t

  val map : ('a -> 'b) -> ('a, 'err) t -> ('b, 'err) t

  val map_err : ('err1 -> 'err2) -> ('a, 'err1) t -> ('a, 'err2) t

  val combine_l : ('a, 'e) result list -> ('a list, 'e list) result

  module Infix : sig
    val ( >|= ) : ('a, 'err) t -> ('a -> 'b) -> ('b, 'err) t

    val ( >>= ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
  end
end

module My_opt : sig
  val return : 'a -> 'a option

  val map : ('a -> 'b) -> 'a option -> 'b option

  val flat_map : ('a -> 'b option) -> 'a option -> 'b option
end

module My_list : sig
  val take : int -> 'a list -> 'a list

  val map : ('a -> 'b) -> 'a list -> 'b list

  val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

  val filter_map : ('a -> 'b option) -> 'a list -> 'b list

  val filter_mapi : (int -> 'a -> 'b option) -> 'a list -> 'b list

  val find_map : ('a -> 'b option) -> 'a list -> 'b option

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
end
