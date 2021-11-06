open Support

(** The type of names for declarations introduced in a signature. *)
type t

(** {1 Constructors} *)

(** [make l s] is the name with location [l] and value [s]. *)
val make : Location.t -> string -> t

(** [make_blank l] is the name with location [l] and value ["_"]. *)
val make_blank : Location.t -> t

(** {1 Destructors} *)

(** [location n] is the location of [n] in a parsed signature. *)
val location : t -> Location.t

(** [value n] is the string value of [n] as found in a parsed signature. *)
val value : t -> string

(** {1 Instances} *)

include Show.SHOW with type t := t
include Eq.EQ with type t := t
include Ord.ORD with type t := t
