open Support

module type ID = sig
  type t

  include Ord.ORD with type t := t

  include Hash.HASH with type t := t

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  module Hamt : Hamt.S with type key = t
end

(** Base implementation for IDs as integers. *)
module BaseId : sig
  (** Unique identifiers for declarations in a signature as integers.

      This module type enables internal modules to construct IDs as integers.
      The type for IDs remains abstract in exported module signatures. *)
  include ID with type t = int

  (** {1 Constructors} *)

  val min_value : t

  val max_value : t

  val next : t -> t
end = struct
  include Int

  let min_value = 0

  let max_value = max_int

  let next = ( + ) 1
end

module Typ = BaseId
module Const = BaseId
module CompTyp = BaseId
module CompConst = BaseId
module CompCotyp = BaseId
module CompDest = BaseId
module Comp = BaseId
module Module = BaseId
module Query = BaseId
module MQuery = BaseId
module Schema = BaseId

module Allocator = struct
  type state =
    { typ : BaseId.t
    ; const : BaseId.t
    ; comp_typ : BaseId.t
    ; comp_const : BaseId.t
    ; comp_cotyp : BaseId.t
    ; comp_dest : BaseId.t
    ; comp : BaseId.t
    ; module_ : BaseId.t
    ; query : BaseId.t
    ; mquery : BaseId.t
    ; schema : BaseId.t
    }

  include (
    State.Make (struct
      type t = state
    end) :
      State.STATE with type state := state)

  let initial_state =
    { typ = BaseId.min_value
    ; const = BaseId.min_value
    ; comp_typ = BaseId.min_value
    ; comp_const = BaseId.min_value
    ; comp_cotyp = BaseId.min_value
    ; comp_dest = BaseId.min_value
    ; comp = BaseId.min_value
    ; module_ = BaseId.min_value
    ; query = BaseId.min_value
    ; mquery = BaseId.min_value
    ; schema = BaseId.min_value
    }

  let next_id getter setter =
    get >>= fun state ->
    let previous_id = getter state in
    if BaseId.(previous_id = max_value) then
      raise @@ Invalid_argument "Exhausted sequence of fresh IDs"
    else
      let next = BaseId.next previous_id in
      put (setter state next) $> Fun.const next

  let next_typ_id =
    next_id (fun { typ; _ } -> typ) (fun state typ -> { state with typ })

  let next_const_id =
    next_id
      (fun { const; _ } -> const)
      (fun state const -> { state with const })

  let next_comp_typ_id =
    next_id
      (fun { comp_typ; _ } -> comp_typ)
      (fun state comp_typ -> { state with comp_typ })

  let next_comp_const_id =
    next_id
      (fun { comp_const; _ } -> comp_const)
      (fun state comp_const -> { state with comp_const })

  let next_comp_cotyp_id =
    next_id
      (fun { comp_cotyp; _ } -> comp_cotyp)
      (fun state comp_cotyp -> { state with comp_cotyp })

  let next_comp_dest_id =
    next_id
      (fun { comp_dest; _ } -> comp_dest)
      (fun state comp_dest -> { state with comp_dest })

  let next_comp_id =
    next_id (fun { comp; _ } -> comp) (fun state comp -> { state with comp })

  let next_module_id =
    next_id
      (fun { module_; _ } -> module_)
      (fun state module_ -> { state with module_ })

  let next_query_id =
    next_id
      (fun { query; _ } -> query)
      (fun state query -> { state with query })

  let next_mquery_id =
    next_id
      (fun { mquery; _ } -> mquery)
      (fun state mquery -> { state with mquery })

  let next_schema_id =
    next_id
      (fun { schema; _ } -> schema)
      (fun state schema -> { state with schema })
end
