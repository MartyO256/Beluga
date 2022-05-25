(** Logic programming query declarations on computational types. *)

open Support
open Common
open Internal

type t

type search_parameters =
  { expected_solutions : int Option.t
  ; search_tries : int Option.t
  ; search_depth : int Option.t
  ; split_index : int Option.t
  }

(** {1 Constructors} *)

val make_search_parameters :
     ?expected_solutions:int
  -> ?search_tries:int
  -> ?search_depth:int
  -> ?split_index:int
  -> unit
  -> search_parameters

val make :
     id:Id.MQuery.t
  -> location:Location.t
  -> ?name:Name.t
  -> ?search_parameters:search_parameters
  -> ?documentation_comment:DocumentationComment.t
  -> Internal.Comp.typ * offset
  -> t

(** {1 Destructors} *)

val id : t -> Id.MQuery.t

val location : t -> Location.t

val name : t -> Name.t Option.t

val query : t -> Comp.typ * offset

val search_parameters : t -> search_parameters

val documentation_comment : t -> DocumentationComment.t Option.t
