(** Bound variable names.

    These are totally ordered for efficient lookups in map data structures.

    For signatures, a name is typically a string. *)

open Support

(** The type of names for bound variables. *)
type t

(** {1 Collections} *)

module Set : Set.S with type elt = t

module Map : Map.S with type key = t

module Hamt : Hamt.S with type key = t

module LinkedMap : LinkedMap.S with type key = t

module LinkedHamt : Support.LinkedHamt.S with type key = t

module LinkedHamt1 : Support.LinkedHamt.S1 with type key = t

(** {1 Name Generation} *)

(** The type of supplier for a name that does not appear in a given set of
    used names. *)
type fresh_name_supplier = Set.t -> t

(** [prefixed_fresh_name_supplier base] is the fresh name supplier for names
    prefixed by [base] and optionally having an integer suffix. *)
val prefixed_fresh_name_supplier : string -> fresh_name_supplier

(** {1 Instances} *)

include Eq.EQ with type t := t

include Ord.ORD with type t := t

include Show.SHOW with type t := t
