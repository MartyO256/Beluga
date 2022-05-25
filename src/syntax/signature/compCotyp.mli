(** Computation-level codata type constant declarations. *)

open Support
open Common
open Internal

type t

(** {1 Constructors} *)

val make_initial_declaration :
     id:Id.CompCotyp.t
  -> name:Name.t
  -> location:Location.t
  -> implicit_arguments:int
  -> ?documentation_comment:DocumentationComment.t
  -> Comp.kind
  -> t

(** {1 Destructors} *)

val id : t -> Id.CompCotyp.t

val location : t -> Location.t

val name : t -> Name.t

val kind : t -> Comp.kind

val documentation_comment : t -> DocumentationComment.t Option.t

(** {1 Freezing} *)

exception UnfrozenCompCotyp of t

exception FrozenCompCotyp of t

val is_frozen : t -> bool

val is_unfrozen : t -> bool

(** [freeze cK] is the frozen version of [cK].

    @raise FrozenCompCotyp If [cK] is frozen. *)
val freeze : t -> t

(** {1 Destructors} *)

exception CompCotypNameCollision of Name.t * Id.CompDest.t * t

exception CompDestNameCollision of Name.t * Id.CompDest.t * t

(** [add_destructor name cM cK] adds the destructor [cM] having name [name]
    to the computational type family [cK].

    @raise FrozenCompCotyp If [cK] is frozen.
    @raise CompCotypNameCollision If [cK] and [cM] have the same name.
    @raise CompDestNameCollision
      If there exists a constructor in [cK] having the same name as [cM]. *)
val add_destructor : Name.t -> Id.CompDest.t -> t -> t

val destructors : t -> Id.CompDest.t Name.Hamt.t

val has_destructor_with_name : Name.t -> t -> bool
