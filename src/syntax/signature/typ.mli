(** LF type family declarations. *)

open Support
open Common
open Internal

type t

(** {1 Constructors} *)

val make_initial_declaration :
     id:Id.Typ.t
  -> name:Name.t
  -> location:Location.t
  -> ?documentation_comment:DocumentationComment.t
  -> LF.kind
  -> t

(** {1 Destructors} *)

val id : t -> Id.Typ.t

val location : t -> Location.t

val name : t -> Name.t

val kind : t -> LF.kind

val arguments : t -> int

val implicit_arguments : t -> int

val explicit_arguments : t -> int

val documentation_comment : t -> DocumentationComment.t Option.t

(** {1 Freezing} *)

exception UnfrozenTyp of t

exception FrozenTyp of t

val is_frozen : t -> bool

val is_unfrozen : t -> bool

(** [freeze ~term_subordinates ~type_subordinated_to tA] is the frozen
    version of [tA] using the input subordination information.

    @raise FrozenTyp If [tA] is frozen. *)
val freeze :
     term_subordinates:Id.Typ.Set.t
  -> type_subordinated_to:Id.Typ.Set.t
  -> t
  -> t

(** {1 LF Constructors} *)

exception TypNameCollision of Name.t * Id.Const.t * t

exception ConstNameCollision of Name.t * Id.Const.t * t

(** [add_constructor name tM tA] adds the constructor [tM] having name [name]
    to the type family [tA].

    @raise FrozenTyp If [tA] is frozen.
    @raise TypNameCollision If [tA] and [tM] have the same name.
    @raise ConstNameCollision
      If there exists a constructor in [tA] having the same name as [tM]. *)
val add_constructor : Name.t -> Id.Const.t -> t -> t

val constructors : t -> Id.Const.t Name.Hamt.t

val has_constructor_with_name : Name.t -> t -> bool

(** {1 Naming} *)

val fresh_var_name :
  t -> ?default_base_name:string -> Name.fresh_name_supplier

val fresh_mvar_name :
  t -> ?default_base_name:string -> Name.fresh_name_supplier

val set_var_naming_convention : string Option.t -> t -> t

val set_mvar_naming_convention : string Option.t -> t -> t

val set_naming_conventions :
  var:string Option.t -> mvar:string Option.t -> t -> t

(** {1 Subordination} *)

(** [is_term_subordinate tA tB_id] is [true] if and only if the LF family
    having ID [tB_id] is term-level subordinate to [tA].

    This is determined using the subordination data passed when [tA] was
    frozen with {!freeze}.

    @raise FrozenTyp If [tA] is frozen. *)
val is_term_subordinate : t -> Id.Typ.t -> bool

(** [is_type_subordinate_to tA tB_id] is [true] if and only if the LF family
    [tA] is type-level subordinate to the LF family having ID [tB_id].

    If [tA] is type-level subordinate to [tB], then [tA]-terms can contain
    [tB]-terms. Type-level subordination is not transitive.

    For instance, given the following frozen signature:

    {v
        nat : type.
        list : nat -> type.
        t : list (suc (suc z)) -> type.
        t' : list (suc (suc N)) -> type.
    v}

    We have that:

    - [is_type_subordinate_to list (id nat) = true]
    - [is_type_subordinate_to t (id list) = true]
    - [is_type_subordinate_to t' (id list) = true]
    - [is_type_subordinate_to t' (id nat) = true]

    And:

    - [is_type_subordinate_to nat (id nat) = false]
    - [is_type_subordinate_to nat (id list) = false]
    - [is_type_subordinate_to nat (id t) = false]
    - [is_type_subordinate_to nat (id t') = false]
    - [is_type_subordinate_to list (id t) = false]
    - [is_type_subordinate_to list (id t') = false]
    - [is_type_subordinate_to t (id nat) = false]
    - [is_type_subordinate_to t (id t') = false]
    - [is_type_subordinate_to t' (id t') = false]

    Type-level subordination is determined using the subordination data
    passed when [tA] was frozen with {!freeze}.

    @raise FrozenTyp If [tA] is frozen. *)
val is_type_subordinate_to : t -> Id.Typ.t -> bool
