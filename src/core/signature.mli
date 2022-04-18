(** Beluga signatures.

    @author Marc-Antoine Ouimet *)

open Support
open Id

(** Bound variable names.

    These are totally ordered for efficient lookups in map data structures.

    For signatures, a name is typically a string. *)
module Name : sig
  (** The type of names for bound variables. *)
  type t

  (** {1 Collections} *)

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  module Hamt : Hamt.S with type key = t

  module LinkedMap : LinkedMap.S with type key = t

  module LinkedHamt : Support.LinkedHamt.S with type key = t

  module LinkedHamt1 : Support.LinkedHamt.S1 with type key = t

  (** {1 Name Generation} *)

  (** The type of supplier for a name that does not appear in a given set of
      used names. *)
  type fresh_name_supplier = Set.t -> t

  (** [prefixed_fresh_name_supplier base] is the fresh name supplier for
      names prefixed by [base] and optionally having an integer suffix. *)
  val prefixed_fresh_name_supplier : string -> fresh_name_supplier

  (** {1 Instances} *)

  include Eq.EQ with type t := t

  include Ord.ORD with type t := t

  include Show.SHOW with type t := t
end

(** Namespaced bound variable names.

    These are names for referring to bound variable names nested in modules. *)
module QualifiedName : sig
  (** The type of names for referring to names in the current module or in a
      different module. *)
  type t

  (** The type of names for modules and declarations. *)
  type name

  (** {1 Constructors} *)

  (** [make ms n] is the qualified name with name [n] when successively
      opening the modules named [ms]. *)
  val make : ?modules:name List.t -> name -> t

  (** {1 Destructors} *)

  (** [name n] is the declaration name referred to by [n]. *)
  val name : t -> name

  (** [modules n] is the list of module names for modules to open to refer to
      [n] in the module opening order. *)
  val modules : t -> name List.t

  (** {1 Instances} *)

  include Show.SHOW with type t := t

  include Eq.EQ with type t := t

  include Ord.ORD with type t := t

  (** {1 Collections} *)

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end

(** Bindings of entries to names. *)
module Declaration : sig
  (** The type of declarations for bound variables referring to signature
      entries. *)
  type +'entry t

  (** The type of bound variable names for a declaration.

      This is the domain of signature declarations. *)
  type name

  (** {1 Constructors} *)

  (** [make name entry] is the declaration having name [name] and entry
      [entry]. *)
  val make : name:name -> entry:'entry -> 'entry t

  (** {1 Destructors} *)

  (** [name declaration] is the variable name bound by [declaration]. *)
  val name : 'entry t -> name

  (** [entry declaration] is the entry referred to by [declaration]. *)
  val entry : 'entry t -> 'entry
end

(** Unique identifiers for declarations in a signature.

    An ID uniquely refers to a signature declaration in a source file.
    However, since declarations may be elaborated in steps, derived
    declarations share the same ID.

    IDs are generated when initial entries are introduced to a signature. *)
module type ID = sig
  (** The type of identifiers for signature declarations. *)
  type t

  (** {1 Instances} *)

  include Ord.ORD with type t := t

  (** {1 Collections} *)

  module Set : Set.S with type elt := t

  module Map : Map.S with type key := t
end

(** Beluga declaration identifiers. *)
module Id : sig
  module Typ : ID

  module Const : ID

  module CompTyp : ID

  module CompCotyp : ID

  module CompConst : ID

  module CompDest : ID

  module Comp : ID

  module Module : ID

  module Query : ID

  module MQuery : ID
end

(** LF type family declarations. *)
module Typ : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make_initial_entry :
       id:int
    -> name:Name.t
    -> location:Location.t
    -> implicit_arguments:int
    -> LF.kind
    -> t

  (** {1 Destructors} *)

  val id : t -> Id.Typ.t

  val location : t -> Location.t

  val name : t -> Name.t

  val kind : t -> LF.kind

  (** {1 Freezing} *)

  val is_frozen : t -> bool

  val is_unfrozen : t -> bool

  val freeze :
       subordinates:Id.Typ.Set.t
    -> type_subordinated:Id.Typ.Set.t
    -> t
    -> (t, [> `Frozen_typ_declaration_error of Id.Typ.t ]) Result.t

  (** {1 LF Constructors} *)

  val add_constructor :
       Name.t
    -> Id.Const.t
    -> t
    -> (t, [> `Frozen_typ_declaration_error of Id.Typ.t ]) result

  val constructors : t -> Id.Const.t Name.Map.t

  val has_constructor_with_name : Name.t -> t -> bool

  (** {1 Naming} *)

  val fresh_var_name :
    t -> ?default_base_name:string -> Name.fresh_name_supplier

  val fresh_mvar_name :
    t -> ?default_base_name:string -> Name.fresh_name_supplier

  val set_var_naming_convention : Name.t Option.t -> t -> t

  val set_mvar_naming_convention : Name.t Option.t -> t -> t

  val set_naming_conventions :
    var:Name.t Option.t -> mvar:Name.t Option.t -> t -> t

  (** {1 Subordination} *)

  val is_subordinate :
       t
    -> Id.Typ.t
    -> (bool, [> `Unfrozen_typ_declaration_error of Id.Typ.t ]) Result.t

  val is_type_subordinated :
       t
    -> Id.Typ.t
    -> (bool, [> `Unfrozen_typ_declaration_error of Id.Typ.t ]) Result.t
end

(** LF type constant declarations. *)
module Const : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make :
       id:int
    -> name:string
    -> location:Location.t
    -> implicit_arguments:int
    -> kind:Id.Typ.t
    -> LF.typ
    -> t

  (** {1 Destructors}*)

  val id : t -> Id.Const.t

  val location : t -> Location.t

  val name : t -> Name.t

  val typ : t -> LF.typ

  val kind : t -> Id.Typ.t
end

(** Computation-level data type constant declarations. *)
module CompTyp : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make_initial_entry :
       id:int
    -> name:Name.t
    -> location:Location.t
    -> implicit_arguments:int
    -> positivity:Sgn.positivity_flag
    -> Comp.kind
    -> t

  (** {1 Destructors} *)

  val id : t -> Id.CompTyp.t

  val location : t -> Location.t

  val name : t -> Name.t

  val kind : t -> Comp.kind

  (** {1 Freezing} *)

  val is_frozen : t -> bool

  val is_unfrozen : t -> bool

  val freeze :
       t
    -> (t, [> `Frozen_comp_typ_declaration_error of Id.CompTyp.t ]) Result.t

  (** {1 Constructors} *)

  val add_constructor :
       Name.t
    -> Id.CompConst.t
    -> t
    -> (t, [> `Frozen_comp_typ_declaration_error of Id.CompTyp.t ]) result

  val constructors : t -> Id.CompConst.t Name.Map.t

  val has_constructor_with_name : Name.t -> t -> bool
end

(** Computation-level type constructor declarations. *)
module CompConst : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make :
       id:int
    -> name:Name.t
    -> location:Location.t
    -> implicit_arguments:int
    -> kind:Id.CompTyp.t
    -> Comp.typ
    -> t

  (** {1 Destructors}*)

  val id : t -> Id.Const.t

  val location : t -> Location.t

  val name : t -> Name.t

  val implicit_arguments : t -> int

  val typ : t -> Comp.typ

  val kind : t -> Id.CompTyp.t
end

(** Computation-level codata type constant declarations. *)
module CompCotyp : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make_initial_entry :
       id:int
    -> name:Name.t
    -> location:Location.t
    -> implicit_arguments:int
    -> Comp.kind
    -> t

  (** {1 Destructors} *)

  val id : t -> Id.CompCotyp.t

  val location : t -> Location.t

  val name : t -> Name.t

  val kind : t -> Comp.kind

  (** {1 Freezing} *)

  val is_frozen : t -> bool

  val is_unfrozen : t -> bool

  val freeze :
       t
    -> ( t
       , [> `Frozen_comp_cotyp_declaration_error of Id.CompCotyp.t ] )
       Result.t

  (** {1 Destructors} *)

  val add_destructor :
       Name.t
    -> Id.CompDest.t
    -> t
    -> ( t
       , [> `Frozen_comp_cotyp_declaration_error of Id.CompCotyp.t ] )
       result

  val destructors : t -> Id.CompDest.t Name.Map.t

  val has_destructor_with_name : Name.t -> t -> bool
end

(** Computation-level type destructor declarations. *)
module CompDest : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make :
       id:int
    -> name:Name.t
    -> location:Location.t
    -> implicit_arguments:int
    -> mctx:LF.mctx
    -> observation_typ:Comp.typ
    -> return_typ:Comp.typ
    -> kind:Id.CompCotyp.t
    -> t

  (** {1 Destructors}*)

  val id : t -> Id.Const.t

  val location : t -> Location.t

  val name : t -> Name.t

  val implicit_arguments : t -> int

  val mctx : t -> LF.mctx

  val observation_typ : t -> Comp.typ

  val return_typ : t -> Comp.typ

  val kind : t -> Id.CompCotyp.t
end

(** Computation declarations. *)
module Comp : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make :
       id:int
    -> name:string
    -> location:Location.t
    -> implicit_arguments:int
    -> typ:Comp.typ
    -> ?mutual_group:Id.Comp.t Nonempty.t Option.t
    -> Comp.value
    -> t

  (** {1 Destructors} *)

  val id : t -> Id.Comp.t

  val location : t -> Location.t

  val name : t -> Name.t

  val implicit_arguments : t -> int

  val typ : t -> Comp.typ

  val program : t -> Comp.value

  val mutual_group : t -> Id.Comp.t Nonempty.t Option.t
end

(** Namespace for declarations. *)
module Module : sig
  type 'a t

  (** {1 Constructors} *)

  val make :
       id:int
    -> location:Location.t
    -> ?declarations:'a Name.LinkedHamt.t
    -> Name.t
    -> 'a t

  (** {1 Destructors} *)

  val id : 'a t -> Id.Comp.t

  val location : 'a t -> Location.t

  val name : 'a t -> Name.t

  val declarations : 'a t -> 'a Name.LinkedHamt.t
end

(** Documentation comments.

    These are declared as [%{{ content }}%] in the external syntax. *)
module DocumentationComment : sig
  type t

  (** {1 Constructors} *)

  val make : location:Location.t -> string -> t

  (** {1 Destructors} *)

  val content : t -> string

  val location : t -> Location.t
end

(** Logic programming query declarations on LF type. *)
module Query : sig
  open Syntax.Int

  type t

  type search_parameters =
    { expected_solutions : int Option.t
    ; maximum_tries : int Option.t
    ; search_depth : int Option.t
    }

  (** {1 Constructors} *)

  val make_search_parameters :
       ?expected_solutions:int Option.t
    -> ?maximum_tries:int Option.t
    -> ?search_depth:int Option.t
    -> unit
    -> search_parameters

  val make :
       id:Id.Query.t
    -> location:Location.t
    -> ?name:string Option.t
    -> search_parameters:search_parameters
    -> LF.mctx * (LF.typ * offset)
    -> t

  (** {1 Destructors} *)

  val id : t -> Id.Query.t

  val location : t -> Location.t

  val name : t -> Name.t Option.t

  val query : t -> LF.mctx * (LF.typ * offset)

  val search_parameters : t -> search_parameters

  (** {1 Collections} *)

  module Set : Set.S with type elt = t

  (** {1 Instances} *)

  include Ord.ORD with type t := t
end

(** Logic programming query declarations on computational types. *)
module MQuery : sig
  open Syntax.Int

  type t

  type search_parameters =
    { expected_solutions : int Option.t
    ; search_tries : int Option.t
    ; search_depth : int Option.t
    ; split_index : int Option.t
    }

  (** {1 Constructors} *)

  val make_search_parameters :
       ?expected_solutions:int Option.t
    -> ?search_tries:int Option.t
    -> ?search_depth:int Option.t
    -> ?split_index:int Option.t
    -> unit
    -> search_parameters

  val make :
       id:Id.MQuery.t
    -> location:Location.t
    -> ?name:string Option.t
    -> search_parameters:search_parameters
    -> Synint.Comp.typ * offset
    -> t

  (** {1 Destructors} *)

  val id : t -> Id.Query.t

  val location : t -> Location.t

  val name : t -> Name.t Option.t

  val query : t -> Comp.typ * offset

  val search_parameters : t -> search_parameters

  (** {1 Collections} *)

  module Set : Set.S with type elt = t

  (** {1 Instances} *)

  include Ord.ORD with type t := t
end
