(** Documentation comments.

    These are declared as [%{{ content }}%] in the external syntax. *)

open Support

type t

(** {1 Constructors} *)

val make : location:Location.t -> string -> t

(** {1 Destructors} *)

val content : t -> string

val location : t -> Location.t
