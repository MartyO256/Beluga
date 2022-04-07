(** Hash array mapped tries. *)

include module type of Hamt

(** Totally ordered and hashable types. *)
module type HASH_TYPE = sig
  (** The type of totally ordered and hashable types. *)
  type t

  (** {1 Instances} *)

  include Ord.ORD with type t := t

  include Hash.HASH with type t := t
end

(** Functor building an implementation of the HAMT structure given a totally
    ordered and hashable type for keys. *)
module Make (Key : HASH_TYPE) : S with type key = Key.t
