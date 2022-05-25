(** Computation-level type destructor declarations. *)

open Support
open Common
open Internal

type t

(** {1 Constructors} *)

val make :
     id:Id.CompDest.t
  -> name:Name.t
  -> location:Location.t
  -> implicit_arguments:int
  -> mctx:LF.mctx
  -> observation_typ:Comp.typ
  -> return_typ:Comp.typ
  -> ?documentation_comment:DocumentationComment.t
  -> Id.CompCotyp.t
  -> t

(** {1 Destructors}*)

val id : t -> Id.CompDest.t

val location : t -> Location.t

val name : t -> Name.t

val implicit_arguments : t -> int

val mctx : t -> LF.mctx

val observation_typ : t -> Comp.typ

val return_typ : t -> Comp.typ

val kind : t -> Id.CompCotyp.t

val documentation_comment : t -> DocumentationComment.t Option.t
