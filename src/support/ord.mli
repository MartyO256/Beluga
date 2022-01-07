(** Module type for totally ordered types. *)
module type ORD = sig
  type t

  (** [compare a b] compares [a] and [b] for ordering. It returns

      - a negative number if [a] precedes [b]
      - [0] if [a = b]
      - a positive number if [a] succeeds [b].

      This should satisfy the following properties:

      - {b Comparability}: [(compare a b <= 0 || compare b a) = true]
      - {b Transitivity}: if [compare a b] and [compare b c], then
        [compare a c]
      - {b Reflexivity}: [(compare a a) = true]
      - {b Antisymmetry}: if [(compare a b) <= 0] and [(compare a b) >= 0]
        then [(compare a b) = 0] *)
  val compare : t -> t -> int

  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( >= ) : t -> t -> bool

  (** [max a b] is [a] if [a >= b] and [b] otherwise. *)
  val max : t -> t -> t

  (** [min a b] is [a] if [a <= b] and [b] otherwise. *)
  val min : t -> t -> t
end

(** Functor building an implementation of {!ORD} given a type with a total
    comparator. *)
module Make (T : sig
  type t

  val compare : t -> t -> int
end) : ORD with type t = T.t

(** Functor building an implementation of {!ORD} whose ordering is the
    reverse of the given totally ordered type. *)
module Reverse (Ord : ORD) : ORD with type t = Ord.t
