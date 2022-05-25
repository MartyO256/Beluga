(** Specifications for sets of contexts. *)

open Support
open Common
open Internal

type t

(** {1 Constructors} *)

val make :
     id:Id.Schema.t
  -> name:Name.t
  -> location:Location.t
  -> ?documentation_comment:DocumentationComment.t
  -> LF.schema
  -> t

(** {1 Destructors} *)

val id : t -> Id.Schema.t

val name : t -> Name.t

val location : t -> Location.t

val schema : t -> LF.schema

val documentation_comment : t -> DocumentationComment.t Option.t
