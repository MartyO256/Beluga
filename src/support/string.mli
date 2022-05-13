include module type of Stdlib.String

(** [unpack s] is [s] as a list of characters. *)
val unpack : string -> char list

(** [pack cs] is the string concatenation of [cs]. *)
val pack : char list -> string

(** [drop n s] is the substring of [s] without the first [n] leading
    characters. *)
val drop : int -> string -> string

(** {1 Collections} *)

module Set : Set.S with type elt = t

module Map : Map.S with type key = t

module Hamt : HamtMisc.S with type key = t

(** {1 Instances} *)

include Eq.EQ with type t := t

include Ord.ORD with type t := t

include Hash.HASH with type t := t

include Show.SHOW with type t := t
