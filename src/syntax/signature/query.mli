(** Logic programming query declarations on LF type. *)

open Support
open Common
open Internal

type t

type search_parameters =
  { expected_solutions : int Option.t
  ; maximum_tries : int Option.t
  ; search_depth : int Option.t
  }

(** {1 Constructors} *)

val make_search_parameters :
     ?expected_solutions:int
  -> ?maximum_tries:int
  -> ?search_depth:int
  -> unit
  -> search_parameters

val make :
     id:Id.Query.t
  -> location:Location.t
  -> ?name:Name.t
  -> ?search_parameters:search_parameters
  -> ?documentation_comment:DocumentationComment.t
  -> LF.mctx * (LF.typ * offset)
  -> t

(** {1 Destructors} *)

val id : t -> Id.Query.t

val location : t -> Location.t

val name : t -> Name.t Option.t

val query : t -> LF.mctx * (LF.typ * offset)

val search_parameters : t -> search_parameters

val documentation_comment : t -> DocumentationComment.t Option.t
