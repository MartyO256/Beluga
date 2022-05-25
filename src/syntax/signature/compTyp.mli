(** Computation-level data type constant declarations. *)

open Support
open Common
open Internal

type t

(** {1 Constructors} *)

val make_initial_declaration :
     id:Id.CompTyp.t
  -> name:Name.t
  -> location:Location.t
  -> implicit_arguments:int
  -> positivity:Sgn.positivity_flag
  -> ?documentation_comment:DocumentationComment.t
  -> Comp.kind
  -> t

(** {1 Destructors} *)

val id : t -> Id.CompTyp.t

val location : t -> Location.t

val name : t -> Name.t

val kind : t -> Comp.kind

val documentation_comment : t -> DocumentationComment.t Option.t

(** {1 Freezing} *)

exception UnfrozenCompTyp of t

exception FrozenCompTyp of t

val is_frozen : t -> bool

val is_unfrozen : t -> bool

(** [freeze cK] is the frozen version of [cK].

    @raise FrozenCompTyp If [cK] is frozen. *)
val freeze : t -> t

(** {1 Constructors} *)

exception CompTypNameCollision of Name.t * Id.CompConst.t * t

exception CompConstNameCollision of Name.t * Id.CompConst.t * t

(** [add_constructor name cM cK] adds the constructor [cM] having name [name]
    to the computational type family [cK].

    @raise FrozenCompTyp If [cK] is frozen.
    @raise CompTypNameCollision If [cK] and [cM] have the same name.
    @raise CompConstNameCollision
      If there exists a constructor in [cK] having the same name as [cM]. *)
val add_constructor : Name.t -> Id.CompConst.t -> t -> t

val constructors : t -> Id.CompConst.t Name.Hamt.t

val has_constructor_with_name : Name.t -> t -> bool
