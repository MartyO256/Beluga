(** Type specified in an interactive use of `suffices` *)
type 'a t =
  [ `exact of 'a (* user specified an exact type annotation *)
  | `infer of Location.t (* user specified `_` and expects the type to be known *)
  ]

val map : ('a -> 'b) -> 'a t -> 'b t
