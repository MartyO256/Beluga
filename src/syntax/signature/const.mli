(** LF type constructor declarations. *)

open Support
open Common
open Internal

type t

(** {1 Constructors} *)

val make :
     id:Id.Const.t
  -> name:Name.t
  -> location:Location.t
  -> implicit_arguments:int
  -> kind:Id.Typ.t
  -> ?documentation_comment:DocumentationComment.t
  -> LF.typ
  -> t

(** {1 Destructors}*)

val id : t -> Id.Const.t

val location : t -> Location.t

val name : t -> Name.t

val typ : t -> LF.typ

val kind : t -> Id.Typ.t

val documentation_comment : t -> DocumentationComment.t Option.t
