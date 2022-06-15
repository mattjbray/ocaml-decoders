(** Util module used for native builds (excluded in bs-config.json) *)
module My_result : sig
  type ('good, 'bad) t = ('good, 'bad) result =
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

  val all_some : 'a option list -> 'a list option

  val map : ('a -> 'b) -> 'a list -> 'b list

  val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

  val filter_map : ('a -> 'b option) -> 'a list -> 'b list

  val find_map : ('a -> 'b option) -> 'a list -> 'b option

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

  val append : 'a list -> 'a list -> 'a list

  val ( @ ) : 'a list -> 'a list -> 'a list

  val flat_map : ('a -> 'b list) -> 'a list -> 'b list
end

val with_file_in : string -> (in_channel -> 'a) -> 'a

val read_all : in_channel -> string
