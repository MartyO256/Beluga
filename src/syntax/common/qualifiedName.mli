(** Namespaced bound variable names.

    These are names for referring to bound variable names nested in modules. *)

open Support

(** The type of names for referring to names in the current module or in a
    different module. *)
type t

(** {1 Constructors} *)

(** [make ms n] is the qualified name with name [n] when successively opening
    the modules named [ms]. *)
val make : ?modules:Name.t List.t -> Name.t -> t

(** {1 Destructors} *)

(** [name n] is the declaration name referred to by [n]. *)
val name : t -> Name.t

(** [modules n] is the list of module names for modules to open to refer to
    [n] in the module opening order. *)
val modules : t -> Name.t List.t

(** {1 Instances} *)

include Show.SHOW with type t := t

include Eq.EQ with type t := t

include Ord.ORD with type t := t

(** {1 Collections} *)

module Set : Set.S with type elt = t

module Map : Map.S with type key = t
