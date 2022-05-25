(** Computation-level type constructor declarations. *)

open Support
open Common
open Internal

type t

(** {1 Constructors} *)

val make :
     id:Id.CompConst.t
  -> name:Name.t
  -> location:Location.t
  -> implicit_arguments:int
  -> kind:Id.CompTyp.t
  -> ?documentation_comment:DocumentationComment.t
  -> Comp.typ
  -> t

(** {1 Destructors}*)

val id : t -> Id.CompConst.t

val location : t -> Location.t

val name : t -> Name.t

val implicit_arguments : t -> int

val typ : t -> Comp.typ

val kind : t -> Id.CompTyp.t

val documentation_comment : t -> DocumentationComment.t Option.t
