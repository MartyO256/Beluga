include module type of Stdlib.Result

(** [of_bool b err] is [Error (err ())] if [b = false] and [Ok ()] otherwise. *)
val of_bool : bool -> (unit -> 'e) -> (unit, 'e) t

(** [( >>= )] is an infix synonym of {!bind}. *)
val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

(** [( >>= )] is an infix synonym of {!map}. *)
val ( $> ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
