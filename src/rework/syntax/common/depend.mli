open Support

(** The type for implicit, explicit or inductive values. Either [Implicit],
    [Explicit] or [Inductive].
*)
type t

(** {1 Constructors} *)

(** [implicit] is [Implicit].
*)
val implicit : t

(** [explicit] is [Explicit].
*)
val explicit : t

(** [inductive] is [Inductive].
*)
val inductive : t

(** {1 Predicates and comparisons} *)

(** [max d1 d2] is {!explicit} if [d1] and [d2] are {!explicit}, and {!implicit}
    otherwise.
*)
val max : t -> t -> t

(** [is_implicit d] is [true] if and only if [d] is {!implicit}.
*)
val is_implicit : t -> bool

(** [is_explicit d] is [true] if and only if [d] is {!explicit}.
*)
val is_explicit : t -> bool

(** [is_inductive d] is [true] if and only if [d] is {!inductive}.
*)
val is_inductive : t -> bool

(** [is_explicit d] is [true] if and only if [d] is not {!implicit}.
*)
val is_explicit' : t -> bool

(** {1 Converting} *)

(** [of_plicity p] is {!implicit} if [p] is {!Plicity.implicit}, and {!explicit}
    if [p] is {!Plicity.explicit}.
*)
val of_plicity : Plicity.t -> t

(** [to_plicity d] is {!Plicity.implicit} if [d] is {!implicit}, and
    {!Plicity.explicit} if [d] is {!explicit}.
    @raise Invalid_argument If [d] is {!inductive}.
*)
val to_plicity : t -> Plicity.t

(** [to_plicity d] is {!Plicity.implicit} if [d] is {!implicit}, and
    {!Plicity.explicit} if [d] is {!explicit} or {!inductive}. This is a
    variant of {!to_plicity} that does not throw when [d] is {!inductive}, and
    instead sends it to {!Plicity.explicit}.
*)
val to_plicity' : t -> Plicity.t

(** {1 Destructors} *)

(** [fold ~implicit ~explicit ~inductive d] is [implicit ()] if [d] is
    {!implicit}, [explicit ()] if [d] is {!explicit}, and [inductive ()] if
    [d] is {!inductive}.
*)
val fold :
  implicit:(unit -> 'a) ->
  explicit:(unit -> 'a) ->
  inductive:(unit -> 'a) ->
  t ->
  'a

(** {1 Instances} *)

module Eq : Eq.EQ with type t = t
