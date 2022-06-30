(** Beluga signatures.

    A Beluga signature is an append-only database for declarations. It is an
    immutable data structure, meaning that a mutation to a signature creates
    a new signature. Significant data sharing is in place to ensure good
    performance.

    {2 Scope-Safe Lookups}

    Lookups in a Beluga signature respect scoping information if the
    signature is kept local to the function making the lookup. To jump at an
    earlier point in a signature, an added declaration is stored along with
    the signature up to and including that added declaration.

    {2 Freezable Declarations}

    Grouped declarations are defined in two stages: unfrozen and frozen.

    - An unfrozen declaration may have additions made to it in derived
      signatures.
    - A frozen declaration may not have such additions.

    Grouped declarations include LF type constants {!Typ},
    computational-level data type constants {!CompTyp}, computational-level
    codata type constants {!CompCotyp}, and namespaces {!Module}.

    Freezable declarations are frozen whenever:

    - the freezable declaration goes out of scope,
    - a module declaration is added,
    - a logic programming query is added, or
    - a theorem is added.

    @author Marc-Antoine Ouimet *)

open Support
open Common

(** The type of Beluga signatures. *)
type t

(** The type of entries in Beluga signatures. *)
type entry

(** The subtype of entries having an ID associated with them.

    Declarations may introduce names in the scope or appear in relationships
    with other declarations. *)
type declaration

(** {1 Exceptions} *)

exception UnboundDeclaration of QualifiedName.t * t

(** {2 LF Type Family Exceptions} *)

exception BoundTypId of Id.Typ.t * t

exception UnboundTypId of Id.Typ.t * t

exception UnboundTyp of QualifiedName.t * t

(** {2 LF Type Constructor Exceptions} *)

exception BoundConstId of Id.Const.t * t

exception UnboundConstId of Id.Const.t * t

exception UnboundConst of QualifiedName.t * t

(** {2 Computational-Level Data Type Exceptions} *)

exception BoundCompTypId of Id.CompTyp.t * t

exception UnboundCompTypId of Id.CompTyp.t * t

exception UnboundCompTyp of QualifiedName.t * t

(** {2 Computational-Level Data Type Constructor Exceptions} *)

exception BoundCompConstId of Id.CompConst.t * t

exception UnboundCompConstId of Id.CompConst.t * t

exception UnboundCompConst of QualifiedName.t * t

(** {2 Computational-Level Codata Type Exceptions} *)

exception BoundCompCotypId of Id.CompCotyp.t * t

exception UnboundCompCotypId of Id.CompCotyp.t * t

exception UnboundCompCotyp of QualifiedName.t * t

(** {2 Computational-Level Codata Type Destructor Exceptions} *)

exception BoundCompDestId of Id.CompDest.t * t

exception UnboundCompDestId of Id.CompDest.t * t

exception UnboundCompDest of QualifiedName.t * t

(** {2 Computation Exceptions} *)

exception BoundCompId of Id.Comp.t * t

exception UnboundCompId of Id.Comp.t * t

exception UnboundComp of QualifiedName.t * t

(** {2 LF Schema Exceptions} *)

exception BoundSchemaId of Id.Schema.t * t

exception UnboundSchemaId of Id.Schema.t * t

exception UnboundSchema of QualifiedName.t * t

(** {2 Module Exceptions} *)

exception BoundModuleId of Id.Module.t * t

exception UnboundModuleId of Id.Module.t * t

exception UnboundModule of QualifiedName.t * t

(** {2 Logic Query Exceptions} *)

exception BoundQueryId of Id.Query.t * t

exception UnboundQueryId of Id.Query.t * t

exception UnboundQuery of QualifiedName.t * t

(** {2 Logic Meta-Query Exceptions} *)

exception BoundMQueryId of Id.MQuery.t * t

exception UnboundMQueryId of Id.MQuery.t * t

exception UnboundMQuery of QualifiedName.t * t

(** {1 Constructors} *)

(** The empty Beluga signature. *)
val empty : t

(** [add_typ signature tA] constructs the signature derived from [signature]
    with the addition of the LF type constant [tA] as a top-level
    declaration.

    In the resultant signature, the declaration having the same name as [tA]
    in [signature] is shadowed if there is any, and frozen if applicable, and
    [tA] is added as unfrozen if it is unfrozen.

    @raise BoundTypId *)
val add_typ : t -> Typ.t -> t

(** [add_const signature tM] constructs the signature derived from
    [signature] with the addition of the LF term constant [tM] as a top-level
    declaration.

    In the resultant signature, the declaration having the same name as [tM]
    in [signature] is shadowed if there is any, and frozen if applicable, and
    the target LF type-level constant for [tM] is updated to have [tM] as
    additional constructor.

    @raise BoundConstId
    @raise FrozenTyp
    @raise TypNameCollision
    @raise ConstNameCollision *)
val add_const : t -> Const.t -> t

(** [add_comp_typ signature cA] constructs the signature derived from
    [signature] with the addition of the computational type constant [cA] as
    a top-level declaration.

    In the resultant signature, the declaration having the same name as [cA]
    in [signature] is shadowed if there is any, and frozen if applicable, and
    [cA] is added as unfrozen if it is unfrozen.

    @raise BoundCompConstId *)
val add_comp_typ : t -> CompTyp.t -> t

(** [add_comp_const signature cM] constructs the signature derived from
    [signature] with the addition of the computational constructor constant
    [cM] as a top-level declaration.

    In the resultant signature, the declaration having the same name as [cM]
    in [signature] is shadowed if there is any, and frozen if applicable, and
    the target computational type constant for [cM] is updated to have [cM]
    as additional constructor.

    @raise BoundCompConstId
    @raise FrozenCompTyp
    @raise CompTypNameCollision
    @raise CompConstNameCollision *)
val add_comp_const : t -> CompConst.t -> t

(** [add_comp_cotyp signature cA] constructs the signature derived from
    [signature] with the addition of the computational cotype constant [cA]
    as a top-level declaration.

    In the resultant signature, the declaration having the same name as [cA]
    in [signature] is shadowed if there is any, and frozen if applicable, and
    [cA] is added as unfrozen if it is unfrozen.

    @raise BoundCompCotypId *)
val add_comp_cotyp : t -> CompCotyp.t -> t

(** [add_comp_dest signature cM] constructs the signature derived from
    [signature] with the addition of the computational destructor constant
    [cM] as a top-level declaration.

    In the resultant signature, the declaration having the same name as [cM]
    in [signature] is shadowed if there is any, and frozen if applicable, and
    the target computational cotype constant for [cM] is updated to have [cM]
    as additional destructor.

    @raise BoundCompDestId
    @raise FrozenCompCotyp
    @raise CompCotypNameCollision
    @raise CompDestNameCollision *)
val add_comp_dest : t -> CompDest.t -> t

(** [add_query signature query] constructs the signature derived from
    [signature] with the addition of the logic programming query on [query]
    as a top-level declaration.

    In the resultant signature, all declarations are frozen.

    @raise BoundQueryId *)
val add_query : t -> Query.t -> t

(** [add_mquery signature mquery] constructs the signature derived from
    [signature] with the addition of the logic programming query on
    computational types [mquery] as a top-level declaration.

    In the resultant signature, all declarations are frozen.

    @raise BoundMQueryId *)
val add_mquery : t -> MQuery.t -> t

(** [add_name_pragma signature name_pragma] constructs the signature derived
    from [signature] with the addition of the name pragma [name_pragma] for
    setting new variable naming conventions for witnesses to an LF type
    family.

    @raise UnboundTypId *)
val add_name_pragma : t -> NamePragma.t -> t

(** [add_documentation_comment signature comment] constructs the signature
    derived from [signature] with the addition of the documentation comment
    [comment]. *)
val add_documentation_comment : t -> DocumentationComment.t -> t

(** {1 Lookups} *)

(** Lookups by qualified name allow for looking up the declaration currently
    in scope at a given path. That is, if a lookup results in some
    declaration, then that declaration is in scope for the signature in which
    it was looked up.

    Looked up declarations are associated with the signature up to and
    including that declaration. This allows for subsequent lookups to be made
    on the signature as of that declaration being made.

    Lookups by ID in a signature allow for bypassing scope checks.
    Declaration IDs were allocated during signature reconstruction as
    declarations were added. Since IDs are floating references, then a
    declaration looked up by ID in a signature is its latest version. That
    is, if the declaration associated with a given ID was altered since it
    was first added, then the lookup by ID returns the altered version of the
    declaration.

    Unsafe lookups by ID are functionally equivalent to safe lookups by ID,
    except that exceptions are raised when a lookup fails. These unsafe
    signature lookup functions are intended to be used when it is known that
    the given ID is bound in the signature and has the intended ID kind. The
    exception types they raise are strictly programmer errors, not user
    errors. *)

(** @raise UnboundDeclaration *)
val lookup : t -> QualifiedName.t -> declaration

(** @raise UnboundDeclaration *)
val lookup' : t -> QualifiedName.t -> t * declaration

val lookup_opt : t -> QualifiedName.t -> declaration Option.t

val lookup_opt' : t -> QualifiedName.t -> (t * declaration) Option.t

(** {2 Typ Lookups} *)

(** @raise UnboundTyp *)
val lookup_typ : t -> QualifiedName.t -> Typ.t

(** @raise UnboundTyp *)
val lookup_typ' : t -> QualifiedName.t -> t * Typ.t

val lookup_typ_opt : t -> QualifiedName.t -> Typ.t Option.t

val lookup_typ_opt' : t -> QualifiedName.t -> (t * Typ.t) Option.t

(** @raise UnboundTypId *)
val lookup_typ_by_id : t -> Id.Typ.t -> Typ.t

(** @raise UnboundTypId *)
val lookup_typ_by_id' : t -> Id.Typ.t -> t * Typ.t

val lookup_typ_by_id_opt : t -> Id.Typ.t -> Typ.t Option.t

val lookup_typ_by_id_opt' : t -> Id.Typ.t -> (t * Typ.t) Option.t

(** {2 Const Lookups} *)

(** @raise UnboundConst *)
val lookup_const : t -> QualifiedName.t -> Const.t

(** @raise UnboundConst *)
val lookup_const' : t -> QualifiedName.t -> t * Const.t

val lookup_const_opt : t -> QualifiedName.t -> Const.t Option.t

val lookup_const_opt' : t -> QualifiedName.t -> (t * Const.t) Option.t

(** @raise UnboundConstId *)
val lookup_const_by_id : t -> Id.Const.t -> Const.t

(** @raise UnboundConstId *)
val lookup_const_by_id' : t -> Id.Const.t -> t * Const.t

val lookup_const_by_id_opt : t -> Id.Const.t -> Const.t Option.t

val lookup_const_by_id_opt' : t -> Id.Const.t -> (t * Const.t) Option.t

(** {2 Comp Typ Lookups} *)

(** @raise UnboundCompTyp *)
val lookup_comp_typ : t -> QualifiedName.t -> CompTyp.t

(** @raise UnboundCompTyp *)
val lookup_comp_typ' : t -> QualifiedName.t -> t * CompTyp.t

val lookup_comp_typ_opt : t -> QualifiedName.t -> CompTyp.t Option.t

val lookup_comp_typ_opt' : t -> QualifiedName.t -> (t * CompTyp.t) Option.t

(** @raise UnboundCompTypId *)
val lookup_comp_typ_by_id : t -> Id.CompTyp.t -> CompTyp.t

(** @raise UnboundCompTypId *)
val lookup_comp_typ_by_id' : t -> Id.CompTyp.t -> t * CompTyp.t

val lookup_comp_typ_by_id_opt : t -> Id.CompTyp.t -> CompTyp.t Option.t

val lookup_comp_typ_by_id_opt' :
  t -> Id.CompTyp.t -> (t * CompTyp.t) Option.t

(** {2 Comp Const Lookups} *)

(** @raise UnboundCompConst *)
val lookup_comp_const : t -> QualifiedName.t -> CompConst.t

(** @raise UnboundCompConst *)
val lookup_comp_const' : t -> QualifiedName.t -> t * CompConst.t

val lookup_comp_const_opt : t -> QualifiedName.t -> CompConst.t Option.t

val lookup_comp_const_opt' :
  t -> QualifiedName.t -> (t * CompConst.t) Option.t

(** @raise UnboundCompConstId *)
val lookup_comp_const_by_id : t -> Id.CompConst.t -> CompConst.t

(** @raise UnboundCompConstId *)
val lookup_comp_const_by_id' : t -> Id.CompConst.t -> t * CompConst.t

val lookup_comp_const_by_id_opt : t -> Id.CompConst.t -> CompConst.t Option.t

val lookup_comp_const_by_id_opt' :
  t -> Id.CompConst.t -> (t * CompConst.t) Option.t

(** {2 Comp Cotyp Lookups} *)

(** @raise UnboundCompCotyp *)
val lookup_comp_cotyp : t -> QualifiedName.t -> CompCotyp.t

(** @raise UnboundCompCotyp *)
val lookup_comp_cotyp' : t -> QualifiedName.t -> t * CompCotyp.t

val lookup_comp_cotyp_opt : t -> QualifiedName.t -> CompCotyp.t Option.t

val lookup_comp_cotyp_opt' :
  t -> QualifiedName.t -> (t * CompCotyp.t) Option.t

(** @raise UnboundCompCotypId *)
val lookup_comp_cotyp_by_id : t -> Id.CompCotyp.t -> CompCotyp.t

(** @raise UnboundCompCotypId *)
val lookup_comp_cotyp_by_id' : t -> Id.CompCotyp.t -> t * CompCotyp.t

val lookup_comp_cotyp_by_id_opt : t -> Id.CompCotyp.t -> CompCotyp.t Option.t

val lookup_comp_cotyp_by_id_opt' :
  t -> Id.CompCotyp.t -> (t * CompCotyp.t) Option.t

(** {2 Comp Dest Lookups} *)

(** @raise UnboundCompDest *)
val lookup_comp_dest : t -> QualifiedName.t -> CompDest.t

(** @raise UnboundCompDest *)
val lookup_comp_dest' : t -> QualifiedName.t -> t * CompDest.t

val lookup_comp_dest_opt : t -> QualifiedName.t -> CompDest.t Option.t

val lookup_comp_dest_opt' : t -> QualifiedName.t -> (t * CompDest.t) Option.t

(** @raise UnboundCompDestId *)
val lookup_comp_dest_by_id : t -> Id.CompDest.t -> CompDest.t

(** @raise UnboundCompDestId *)
val lookup_comp_dest_by_id' : t -> Id.CompDest.t -> t * CompDest.t

val lookup_comp_dest_by_id_opt : t -> Id.CompDest.t -> CompDest.t Option.t

val lookup_comp_dest_by_id_opt' :
  t -> Id.CompDest.t -> (t * CompDest.t) Option.t

(** {2 Comp Lookups} *)

(** @raise UnboundComp *)
val lookup_comp : t -> QualifiedName.t -> Comp.t

(** @raise UnboundComp *)
val lookup_comp' : t -> QualifiedName.t -> t * Comp.t

val lookup_comp_opt : t -> QualifiedName.t -> Comp.t Option.t

val lookup_comp_opt' : t -> QualifiedName.t -> (t * Comp.t) Option.t

(** @raise UnboundCompId *)
val lookup_comp_by_id : t -> Id.Comp.t -> Comp.t

(** @raise UnboundCompId *)
val lookup_comp_by_id' : t -> Id.Comp.t -> t * Comp.t

val lookup_comp_by_id_opt : t -> Id.Comp.t -> Comp.t Option.t

val lookup_comp_by_id_opt' : t -> Id.Comp.t -> (t * Comp.t) Option.t

(** {2 Schema Lookups} *)

(** @raise UnboundSchema *)
val lookup_schema : t -> QualifiedName.t -> Schema.t

(** @raise UnboundSchema *)
val lookup_schema' : t -> QualifiedName.t -> t * Schema.t

val lookup_schema_opt : t -> QualifiedName.t -> Schema.t Option.t

val lookup_schema_opt' : t -> QualifiedName.t -> (t * Schema.t) Option.t

(** @raise UnboundSchemaId *)
val lookup_schema_by_id : t -> Id.Schema.t -> Schema.t

(** @raise UnboundSchemaId *)
val lookup_schema_by_id' : t -> Id.Schema.t -> t * Schema.t

val lookup_schema_by_id_opt : t -> Id.Schema.t -> Schema.t Option.t

val lookup_schema_by_id_opt' : t -> Id.Schema.t -> (t * Schema.t) Option.t

(** {2 Module Lookups} *)

(** @raise UnboundModule *)
val lookup_module : t -> QualifiedName.t -> (t, entry, declaration) Module.t

(** @raise UnboundModule *)
val lookup_module' :
  t -> QualifiedName.t -> t * (t, entry, declaration) Module.t

val lookup_module_opt :
  t -> QualifiedName.t -> (t, entry, declaration) Module.t Option.t

val lookup_module_opt' :
  t -> QualifiedName.t -> (t * (t, entry, declaration) Module.t) Option.t

(** @raise UnboundModuleId *)
val lookup_module_by_id :
  t -> Id.Module.t -> (t, entry, declaration) Module.t

(** @raise UnboundModuleId *)
val lookup_module_by_id' :
  t -> Id.Module.t -> t * (t, entry, declaration) Module.t

val lookup_module_by_id_opt :
  t -> Id.Module.t -> (t, entry, declaration) Module.t Option.t

val lookup_module_by_id_opt' :
  t -> Id.Module.t -> (t * (t, entry, declaration) Module.t) Option.t

(** {2 Query Lookups} *)

(** @raise UnboundQuery *)
val lookup_query : t -> QualifiedName.t -> Query.t

(** @raise UnboundQuery *)
val lookup_query' : t -> QualifiedName.t -> t * Query.t

val lookup_query_opt : t -> QualifiedName.t -> Query.t Option.t

val lookup_query_opt' : t -> QualifiedName.t -> (t * Query.t) Option.t

(** @raise UnboundQueryId *)
val lookup_query_by_id : t -> Id.Query.t -> Query.t

(** @raise UnboundQueryId *)
val lookup_query_by_id' : t -> Id.Query.t -> t * Query.t

val lookup_query_by_id_opt : t -> Id.Query.t -> Query.t Option.t

val lookup_query_by_id_opt' : t -> Id.Query.t -> (t * Query.t) Option.t

(** {2 MQuery Lookups} *)

(** @raise UnboundMQuery *)
val lookup_mquery : t -> QualifiedName.t -> MQuery.t

(** @raise UnboundMQuery *)
val lookup_mquery' : t -> QualifiedName.t -> t * MQuery.t

val lookup_mquery_opt : t -> QualifiedName.t -> MQuery.t Option.t

val lookup_mquery_opt' : t -> QualifiedName.t -> (t * MQuery.t) Option.t

(** @raise UnboundMQueryId *)
val lookup_mquery_by_id : t -> Id.MQuery.t -> MQuery.t

(** @raise UnboundMQueryId *)
val lookup_mquery_by_id' : t -> Id.MQuery.t -> t * MQuery.t

val lookup_mquery_by_id_opt : t -> Id.MQuery.t -> MQuery.t Option.t

val lookup_mquery_by_id_opt' : t -> Id.MQuery.t -> (t * MQuery.t) Option.t

(** {1 Scanning} *)

val find_all_queries : t -> (t * Query.t) List.t

val find_all_mqueries : t -> (t * MQuery.t) List.t

(** {1 Entry Kinds} *)

module DocumentationComment = DocumentationComment
module Typ = Typ
module Const = Const
module CompTyp = CompTyp
module CompConst = CompConst
module CompCotyp = CompCotyp
module CompDest = CompDest
module Comp = Comp
module Module = Module
module Query = Query
module MQuery = MQuery
module NamePragma = NamePragma
