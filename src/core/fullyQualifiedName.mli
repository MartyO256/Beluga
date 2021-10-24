open Support

(** The type of names for referring to names in the current module or in a
    different module.
*)
type t

(** {1 Constructors} *)

(** [make l ms s] is the fully qualified name with location [l] and value [s]
    when successively opening the modules [ms].
*)
val make : Location.t -> string list -> string -> t

(** {1 Destructors} *)

(** [location n] is the location of [n] in a parsed signature.
*)
val location : t -> Location.t

(** [value n] is the tail string value of [n], the referred name as found in the
    module it is declared in.
*)
val value : t -> string

(** {1 Instances} *)

module Show : Show.SHOW with type t = t

module Eq : Eq.EQ with type t = t

module Ord : Ord.ORD with type t = t
