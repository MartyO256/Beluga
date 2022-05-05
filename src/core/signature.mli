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

  (** {1 Constructors} *)

  (** [make ms n] is the qualified name with name [n] when successively
      opening the modules named [ms]. *)
  val make : ?modules:Name.t List.t -> Name.t -> t

  (** {1 Destructors} *)

  (** [name n] is the declaration name referred to by [n]. *)
  val name : t -> Name.t

  (** [modules n] is the list of module names for modules to open to refer to
      [n] in the module opening order. *)
  val modules : t -> Name.t List.t

  (** {1 Instances} *)

  include Show.SHOW with type t := t

  include Eq.EQ with type t := t

  include Ord.ORD with type t := t

  (** {1 Collections} *)

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end

(** Unique identifiers (IDs) for declarations in a signature.

    An ID uniquely refers to a signature declaration in a source file.
    However, since declarations may be elaborated in steps, derived
    declarations share the same ID.

    IDs are generated sequentially using an allocator during signature
    reconstruction. *)
module type ID = sig
  (** The type of identifiers for signature declarations. *)
  type t

  (** {1 Instances} *)

  include Eq.EQ with type t := t

  include Ord.ORD with type t := t

  include Hash.HASH with type t := t

  (** {1 Collections} *)

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  module Hamt : Hamt.S with type key = t
end

(** Beluga declaration identifiers (IDs). *)
module Id : sig
  (** {1 ID Classes} *)

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

  module Schema : ID

  (** The tagged union type of Beluga IDs. *)
  type t =
    [ `Typ_id of Typ.t
    | `Const_id of Const.t
    | `Comp_typ_id of CompTyp.t
    | `Comp_cotyp_id of CompCotyp.t
    | `Comp_const_id of CompConst.t
    | `Comp_dest_id of CompDest.t
    | `Comp_id of Comp.t
    | `Module_id of Module.t
    | `Query_id of Query.t
    | `MQuery_id of MQuery.t
    | `Schema_id of Schema.t
    ]

  (** {1 Constructors} *)

  (** Stateful builder pattern for sequentially making distinct IDs. *)
  module Allocator : sig
    type id = t

    (** Instance of the state monad for the integer value of the latest
        allocated ID. *)
    include State.STATE

    (** [initial_state] is the allocator state with [0] as the latest
        allocated ID. *)
    val initial_state : state

    (** {1 ID Builders} *)

    (** [next_typ_id] is an ID allocator whose next ID is an LF type family
        ID. *)
    val next_typ_id : Typ.t t

    (** [next_const_id] is an ID allocator whose next ID is an LF type
        constant ID. *)
    val next_const_id : Const.t t

    (** [next_comp_typ_id] is an ID allocator whose next ID is a
        computation-level data type constant ID. *)
    val next_comp_typ_id : CompTyp.t t

    (** [next_comp_const_id] is an ID allocator whose next ID is a
        computation-level type constructor ID. *)
    val next_comp_const_id : CompConst.t t

    (** [next_comp_cotyp_id] is an ID allocator whose next ID is a
        computation-level codata type constant ID. *)
    val next_comp_cotyp_id : CompCotyp.t t

    (** [next_comp_dest_id] is an ID allocator whose next ID is a
        computation-level type destructor. *)
    val next_comp_dest_id : CompDest.t t

    (** [next_comp_id] is an ID allocator whose next ID is a computation ID. *)
    val next_comp_id : Comp.t t

    (** [next_module_id] is an ID allocator whose next ID is a module ID. *)
    val next_module_id : Module.t t

    (** [next_query_id] is an ID allocator whose next ID is an ID for a logic
        programming query on LF types. *)
    val next_query_id : Query.t t

    (** [next_mquery_id] is an ID allocator whose next ID is an ID for a
        logic programming query on computational types. *)
    val next_mquery_id : MQuery.t t

    (** [next_schema_id] is an ID allocator whose next ID is a schema ID. *)
    val next_schema_id : Schema.t t
  end

  val lift_typ_id : Typ.t -> t

  val lift_const_id : Const.t -> t

  val lift_comp_typ_id : CompTyp.t -> t

  val lift_comp_const_id : CompConst.t -> t

  val lift_comp_cotyp_id : CompCotyp.t -> t

  val lift_comp_dest_id : CompDest.t -> t

  val lift_comp_id : Comp.t -> t

  val lift_module_id : Module.t -> t

  val lift_query_id : Query.t -> t

  val lift_mquery_id : MQuery.t -> t

  val lift_schema_id : Schema.t -> t

  (** {1 Instances} *)

  include Eq.EQ with type t := t

  include Ord.ORD with type t := t

  include Hash.HASH with type t := t

  (** {1 Collections} *)

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  module Hamt : Hamt.S with type key = t
end

(** LF type family declarations. *)
module Typ : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make_initial_entry :
       id:Id.Typ.t
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
    -> ( t
       , [> `Kind_name_collision of Name.t * Id.Const.t * t
         | `Constructor_name_collision of Name.t * Id.Const.t * t
         | `Frozen_typ_declaration_error of Id.Typ.t
         ] )
       result

  val constructors : t -> Id.Const.t Name.Hamt.t

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
       id:Id.Const.t
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
       id:Id.CompTyp.t
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

  val constructors : t -> Id.CompConst.t Name.Hamt.t

  val has_constructor_with_name : Name.t -> t -> bool
end

(** Computation-level type constructor declarations. *)
module CompConst : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make :
       id:Id.CompConst.t
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
       id:Id.CompCotyp.t
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

  val destructors : t -> Id.CompDest.t Name.Hamt.t

  val has_destructor_with_name : Name.t -> t -> bool
end

(** Computation-level type destructor declarations. *)
module CompDest : sig
  open Syntax.Int

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
       id:Id.Comp.t
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

(** Specifications for sets of contexts. *)
module Schema : sig
  open Syntax.Int

  type t

  val make :
    id:Id.Schema.t -> name:Name.t -> location:Location.t -> LF.schema -> t

  val id : t -> Id.Schema.t

  val name : t -> Name.t

  val location : t -> Location.t

  val schema : t -> LF.schema
end

(** Namespace for declarations. *)
module Module : sig
  type 'a t

  (** {1 Constructors} *)

  val make_empty : id:Id.Module.t -> location:Location.t -> Name.t -> 'a t

  val add_declaration : 'a t -> 'a -> 'a t

  val add_named_declaration : 'a t -> Name.t -> 'a -> 'a t

  (** {1 Destructors} *)

  val id : 'a t -> Id.Comp.t

  val location : 'a t -> Location.t

  val name : 'a t -> Name.t

  val declarations : 'a t -> 'a List.t

  (** {1 Lookups} *)

  val lookup : 'a t -> Name.t -> 'a Option.t
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
    -> ?search_parameters:search_parameters
    -> LF.mctx * (LF.typ * offset)
    -> t

  (** {1 Destructors} *)

  val id : t -> Id.Query.t

  val location : t -> Location.t

  val name : t -> Name.t Option.t

  val query : t -> LF.mctx * (LF.typ * offset)

  val search_parameters : t -> search_parameters
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
    -> ?search_parameters:search_parameters
    -> Synint.Comp.typ * offset
    -> t

  (** {1 Destructors} *)

  val id : t -> Id.Query.t

  val location : t -> Location.t

  val name : t -> Name.t Option.t

  val query : t -> Comp.typ * offset

  val search_parameters : t -> search_parameters
end

(** The type of Beluga signatures. *)
type t

type mutually_recursive_typs =
  [ `Typs of (Typ.t * Const.t Name.LinkedHamt.t) Nonempty.t ]

type mutually_recursive_comp_typs =
  [ `Comp_typs of
    [ `Comp_typ of CompTyp.t * CompConst.t Name.LinkedHamt.t
    | `Comp_cotyp of CompCotyp.t * CompDest.t Name.LinkedHamt.t
    ]
    Nonempty.t
  ]

type mutually_recursive_programs = [ `Programs of Comp.t Name.LinkedHamt1.t ]

(** The type of declarations in Beluga signatures. *)
type declaration =
  [ `Typ_declaration of Typ.t
  | `Const_declaration of Const.t
  | `Comp_typ_declaration of CompTyp.t
  | `Comp_const_declaration of CompConst.t
  | `Comp_cotyp_declaration of CompCotyp.t
  | `Comp_dest_declaration of CompDest.t
  | `Comp_declaration of Comp.t
  | `Schema_declaration of Schema.t
  | `Module_declaration of (t * declaration) Module.t
  | `Documentation_comment of DocumentationComment.t
  | `Mutually_recursive_declaration of
    [ mutually_recursive_typs
    | mutually_recursive_comp_typs
    | mutually_recursive_programs
    ]
  | `Query_declaration of Query.t
  | `MQuery_declaration of MQuery.t
  ]

(** {1 Lookups by Qualified Name} *)

(** Lookups by qualified name allow for looking up the entry currently in
    scope at a given path. That is, if a lookup results in some declaration,
    then that declaration is in scope for the signature in which it was
    looked up. *)

(** [lookup signature name] returns [None] if there is no declaration in
    [signature] having name [name], and otherwise returns
    [Some (signature', declaration)] where [signature'] is the signature up
    to and including [declaration] and [declaration] is the latest
    declaration in [signature] having name [name]. *)
val lookup : t -> QualifiedName.t -> (t * declaration) Option.t

val lookup_typ : t -> QualifiedName.t -> (t * Typ.t) Option.t

val lookup_constructor : t -> QualifiedName.t -> (t * Const.t) Option.t

val lookup_comp_typ : t -> QualifiedName.t -> (t * CompTyp.t) Option.t

val lookup_comp_constructor :
  t -> QualifiedName.t -> (t * CompConst.t) Option.t

val lookup_comp_cotyp : t -> QualifiedName.t -> (t * CompCotyp.t) Option.t

val lookup_comp_destructor :
  t -> QualifiedName.t -> (t * CompDest.t) Option.t

val lookup_comp : t -> QualifiedName.t -> (t * Comp.t) Option.t

val lookup_schema : t -> QualifiedName.t -> (t * Schema.t) Option.t

val lookup_module :
  t -> QualifiedName.t -> (t * (t * declaration) Module.t) Option.t

val lookup_query : t -> QualifiedName.t -> (t * Query.t) Option.t

val lookup_mquery : t -> QualifiedName.t -> (t * MQuery.t) Option.t

(** {1 Lookups by ID} *)

(** Lookups by ID in a signature allows for bypassing scope checks.
    Declaration IDs were allocated during signature reconstruction as
    declarations were added. Since IDs are floating references, then a
    declaration looked up by ID in a signature is its latest version. That
    is, if the declaration associated with a given ID was altered since it
    was first added, then the lookup by ID returns the altered version of the
    declaration. *)

val lookup_typ_by_id : t -> Id.Typ.t -> (t * Typ.t) Option.t

val lookup_constructor_by_id : t -> Id.Const.t -> (t * Const.t) Option.t

val lookup_comp_typ_by_id : t -> Id.CompTyp.t -> (t * CompTyp.t) Option.t

val lookup_comp_constructor_by_id :
  t -> Id.CompConst.t -> (t * CompConst.t) Option.t

val lookup_comp_cotyp_by_id :
  t -> Id.CompCotyp.t -> (t * CompCotyp.t) Option.t

val lookup_comp_destructor_by_id :
  t -> Id.CompDest.t -> (t * CompDest.t) Option.t

val lookup_comp_by_id : t -> Id.Comp.t -> (t * Comp.t) Option.t

val lookup_schema_by_id : t -> Id.Schema.t -> (t * Schema.t) Option.t

val lookup_module_by_id :
  t -> Id.Module.t -> (t * (t * declaration) Module.t) Option.t

val lookup_query_by_id : t -> Id.Query.t -> (t * Query.t) Option.t

val lookup_mquery_by_id : t -> Id.MQuery.t -> (t * MQuery.t) Option.t

(** {1 Unsafe lookups by ID} *)

(** Unsafe lookups by ID are functionally equivalent to safe lookups by ID,
    except that exceptions are raised when a lookup fails.

    These unsafe signature lookup functions are intended to be used when it
    is known that the given ID is bound in the signature and has the intended
    ID kind.

    The exception types they raise are strictly programmer errors, not user
    errors. *)

(** [UnboundId (id, signature)] is the exception raised when [id] could not
    be found in [signature]. *)
exception UnboundId of Id.t * t

type id_kind_mismatch =
  { bound : Id.t
  ; expected : Id.t
  ; signature : t
  }

(** [IdKindMismatch { bound; expected; signature }] is the exception raised
    when IDs [bound] and [expected] differ in the kind of ID looked up in
    [signature]. *)
exception IdKindMismatch of id_kind_mismatch

(** [lookup_typ_by_id_exn signature id] is the LF type family having ID [id]
    in [signature].

    @raise UnboundId If the ID [id] is not in [signature].
    @raise IdKindMismatch
      If the entry having the ID [id] in [signature] is not an LF type
      family. *)
val lookup_typ_by_id_exn : t -> Id.Typ.t -> t * Typ.t

(** [lookup_constructor_by_id_exn signature id] is the LF constant having ID
    [id] in [signature].

    @raise UnboundId If the ID [id] is not in [signature].
    @raise IdKindMismatch
      If the entry having the ID [id] in [signature] is not an LF constant. *)
val lookup_constructor_by_id_exn : t -> Id.Const.t -> t * Const.t

(** [lookup_comp_typ_by_id_exn signature id] is the computational type having
    ID [id] in [signature].

    @raise UnboundId If the ID [id] is not in [signature].
    @raise IdKindMismatch
      If the entry having the ID [id] in [signature] is not a computational
      type. *)
val lookup_comp_typ_by_id_exn : t -> Id.CompTyp.t -> t * CompTyp.t

(** [lookup_comp_constructor_by_id_exn signature id] is the computational
    type constructor having ID [id] in [signature].

    @raise UnboundId If the ID [id] is not in [signature].
    @raise IdKindMismatch
      If the entry having the ID [id] in [signature] is not a computational
      type constructor. *)
val lookup_comp_constructor_by_id_exn :
  t -> Id.CompConst.t -> t * CompConst.t

(** [lookup_comp_cotyp_by_id_exn signature id] is the computational cotype
    having ID [id] in [signature].

    @raise UnboundId If the ID [id] is not in [signature].
    @raise IdKindMismatch
      If the entry having the ID [id] in [signature] is not a computational
      cotype. *)
val lookup_comp_cotyp_by_id_exn : t -> Id.CompCotyp.t -> t * CompCotyp.t

(** [lookup_comp_destructor_by_id_exn signature id] is the computational
    cotype destructor having ID [id] in [signature].

    @raise UnboundId If the ID [id] is not in [signature].
    @raise IdKindMismatch
      If the entry having the ID [id] in [signature] is not a computational
      cotype destructor. *)
val lookup_comp_destructor_by_id_exn : t -> Id.CompDest.t -> t * CompDest.t

(** [lookup_comp_by_id_exn signature id] is the computation having ID [id] in
    [signature].

    @raise UnboundId If the ID [id] is not in [signature].
    @raise IdKindMismatch
      If the entry having the ID [id] in [signature] is not a computation. *)
val lookup_comp_by_id_exn : t -> Id.Comp.t -> t * Comp.t

(** [lookup_schema_by_id_exn signature id] is the schema having ID [id] in
    [signature].

    @raise UnboundId If the ID [id] is not in [signature].
    @raise IdKindMismatch
      If the entry having the ID [id] in [signature] is not a schema. *)
val lookup_schema_by_id_exn : t -> Id.Schema.t -> t * Schema.t

(** [lookup_module_by_id_exn signature id] is the module having ID [id] in
    [signature].

    @raise UnboundId If the ID [id] is not in [signature].
    @raise IdKindMismatch
      If the entry having the ID [id] in [signature] is not a module. *)
val lookup_module_by_id_exn :
  t -> Id.Query.t -> t * (t * declaration) Module.t

(** [lookup_query_by_id_exn signature id] is the logic programming query
    having ID [id] in [signature].

    @raise UnboundId If the ID [id] is not in [signature].
    @raise IdKindMismatch
      If the entry having the ID [id] in [signature] is not a logic
      programming query. *)
val lookup_query_by_id_exn : t -> Id.Query.t -> t * Query.t

(** [lookup_query_by_id_exn signature id] is the logic programming meta-query
    having ID [id] in [signature].

    @raise UnboundId If the ID [id] is not in [signature].
    @raise IdKindMismatch
      If the entry having the ID [id] in [signature] is not a logic
      programming meta-query. *)
val lookup_mquery_by_id_exn : t -> Id.MQuery.t -> t * MQuery.t

(** {1 Declaration IDs} *)

(** [id_of_declaration declaration] is [Some id] with [id] being the lifted
    ID of [declaration] if one is found, and [None] otherwise. *)
val id_of_declaration : [< declaration ] -> Id.t Option.t

(** Exception raised when an ID cannot be found for a declaration. *)
exception DeclarationWithoutId of declaration

(** [id_of_declaration_exn declaration] is the lifted ID of [declaration].

    @raise DeclarationWithoutId
      If there is no ID associated with [declaration]. *)
val id_of_declaration_exn : [< declaration ] -> Id.t

(** {1 Paths} *)

(** Qualified names define paths to entries in a signature.

    In a signature, a path to an entry is valid if it is not shadowed by a
    later and equal path to a different entry. *)

(** [is_path_to_entry signature id path] is [Some (signature', declaration)]
    if looking up the entry in [signature] by ID [id] or qualified name
    [path] result in the same declaration [declaration], with [signature']
    being the signature up to and including [declaration]. If the looked up
    declarations differ or do not exist, then [None] is returned. *)
val is_path_to_entry :
  t -> Id.t -> QualifiedName.t -> (t * declaration) Option.t

(** [all_paths_to_entry signature id] is the set of all qualified names in
    scope that may be used to refer to the declaration having ID [id] in
    [signature]. *)
val all_paths_to_entry :
  t -> Id.t -> (QualifiedName.Set.t, [> `Unbound_id of Id.t * t ]) Result.t

(** [all_paths_to_entry_exn signature id] is
    [all_paths_to_entry signature id], but a programmer error is raised if
    [id] is not bound in [signature]. *)
val all_paths_to_entry_exn : t -> Id.t -> QualifiedName.Set.t

