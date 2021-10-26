open Support

(** The type for implicit or explicit values. Either [Implicit] or [Explicit].
*)
type t

(** {1 Constructors} *)

(** [implicit] is [Implicit].
*)
val implicit : t

(** [explicit] is [Explicit].
*)
val explicit : t

(** {1 Predicates and comparisons} *)

(** [is_explicit p] is [true] if and only if [p] is {!explicit}.
*)
val is_explicit : t -> bool

(** [is_implicit p] is [true] if and only if [p] is {!implicit}.
*)
val is_implicit : t -> bool

(** {1 Destructors} *)

(** [fold ~implicit ~explicit p] is [implicit ()] if [p] is {!implicit} and
    [explicit ()] if [p] is {!explicit}.
*)
val fold : implicit:(unit -> 'a) -> explicit:(unit -> 'a) -> t -> 'a

(** {1 Instances} *)

module Eq : Eq.EQ with type t = t
