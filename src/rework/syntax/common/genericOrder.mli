type 'a t =
  | Arg of 'a
  | Lex of 'a t list
  | Simul of 'a t list

val map : ('a -> 'b) -> 'a t -> 'b t
