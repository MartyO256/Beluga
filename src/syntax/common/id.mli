(** @author Marc-Antoine Ouimet *)

open Support

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

(** {1 ID Kinds} *)

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

(** {1 ID Allocation} *)

(** Stateful builder pattern for sequentially making distinct IDs. *)
module Allocator : sig
  (** Instance of the state monad for the integer value of the latest
      allocated ID. *)
  include State.STATE

  (** [initial_state] is the allocator state with [0] as the latest allocated
      ID. *)
  val initial_state : state

  (** {1 ID Builders} *)

  (** [next_typ_id] is an ID allocator whose next ID is an LF type family ID. *)
  val next_typ_id : Typ.t t

  (** [next_const_id] is an ID allocator whose next ID is an LF type constant
      ID. *)
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

  (** [next_mquery_id] is an ID allocator whose next ID is an ID for a logic
      programming query on computational types. *)
  val next_mquery_id : MQuery.t t

  (** [next_schema_id] is an ID allocator whose next ID is a schema ID. *)
  val next_schema_id : Schema.t t
end
