(** Module type for types with a structural equality predicate and operators. *)
module type EQ = sig
  type t

  (** [equal a b] is [true] if and only if [a] and [b] are structurally
      equal. This should satisfy the following properties:

      - {b Reflexivity}: [(equal a a) = true]
      - {b Symmetry}: [equal a b] is equivalent to [equal b a]
      - {b Transitivity}: if [equal a b] and [equal b c], then [equal a c] *)
  val equal : t -> t -> bool

  (** Operator alias of {!equal}. *)
  val ( = ) : t -> t -> bool

  (** Negation of operator {!(=)}. *)
  val ( <> ) : t -> t -> bool
end

(** Functor building an implementation of {!EQ} given a type with a
    structural equality function.*)
module Make (T : sig
  type t

  val equal : t -> t -> bool
end) : EQ with type t = T.t
