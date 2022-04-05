module type S = sig
  type key

  type +'a t

  val empty : 'a t

  val is_empty : 'a t -> bool

  val mem : key -> 'a t -> bool

  val add : key -> 'a -> 'a t -> 'a t

  val singleton : key -> 'a -> 'a t

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val iter_bindings : (key -> 'a -> unit) -> 'a t -> unit

  val fold_left : ('a -> key -> 'b -> 'a) -> 'b t -> 'a -> 'a

  val fold_right : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val fold_bindings : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val exists : (key -> 'a -> bool) -> 'a t -> bool

  val for_all : (key -> 'a -> bool) -> 'a t -> bool

  val find_opt : key -> 'a t -> 'a option
end

module Make (Map : Map.S) : S with type key = Map.key
