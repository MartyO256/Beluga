(** Computation declarations. *)

open Support
open Common
open Internal

type t

(** {1 Constructors} *)

val make :
     id:Id.Comp.t
  -> name:Name.t
  -> location:Location.t
  -> implicit_arguments:int
  -> typ:Comp.typ
  -> ?mutual_group:Id.Comp.t List1.t
  -> ?documentation_comment:DocumentationComment.t
  -> Comp.value
  -> t

(** {1 Destructors} *)

val id : t -> Id.Comp.t

val location : t -> Location.t

val name : t -> Name.t

val implicit_arguments : t -> int

val typ : t -> Comp.typ

val program : t -> Comp.value

val mutual_group : t -> Id.Comp.t List1.t Option.t

val documentation_comment : t -> DocumentationComment.t Option.t
