open Support
open Beluga_syntax

exception Unknown_constant_arity of Qualified_identifier.t

exception
  Invalid_prefix_arity of
    { constant : Qualified_identifier.t
    ; actual : Int.t
    }

exception
  Invalid_infix_arity of
    { constant : Qualified_identifier.t
    ; actual : Int.t
    }

exception
  Invalid_postfix_arity of
    { constant : Qualified_identifier.t
    ; actual : Int.t
    }

let () =
  Error.register_exception_printer (function
    | Unknown_constant_arity constant ->
        Format.dprintf "Can't determine the arity of constant %a."
          Qualified_identifier.pp constant
    | Invalid_prefix_arity { constant; actual } ->
        Format.dprintf
          "Invalid prefix pragma.@ Prefix operators must have arity 1, but \
           constant %a has arity %d."
          Qualified_identifier.pp constant actual
    | Invalid_infix_arity { constant; actual } ->
        Format.dprintf
          "Invalid infix pragma.@ Infix operators must have arity 2, but \
           constant %a has arity %d."
          Qualified_identifier.pp constant actual
    | Invalid_postfix_arity { constant; actual } ->
        Format.dprintf
          "Invalid postfix pragma.@ Postfix operators must have arity 1, \
           but constant %a has arity %d."
          Qualified_identifier.pp constant actual
    | exn -> Error.raise_unsupported_exception_printing exn)

module type SIGNATURE_RECONSTRUCTION_STATE = sig
  include State.STATE

  val get_leftover_vars :
    (Abstract.free_var Synint.LF.ctx * Location.t) List.t t

  val add_leftover_vars :
    Abstract.free_var Synint.LF.ctx -> Location.t -> Unit.t t

  val with_disabled_lf_strengthening : location:Location.t -> 'a t -> 'a t

  val with_warn_on_coverage_error : location:Location.t -> 'a t -> 'a t

  val with_coverage_checking : location:Location.t -> 'a t -> 'a t

  val set_name_generation_bases :
       location:Location.t
    -> meta_variable_base:Identifier.t
    -> ?computation_variable_base:Identifier.t
    -> Qualified_identifier.t
    -> Unit.t t

  val set_default_associativity :
    ?location:Location.t -> Associativity.t -> Unit.t t

  val get_default_associativity : Associativity.t t

  val set_default_precedence : ?location:Location.t -> Int.t -> Unit.t t

  val get_default_precedence : Int.t t

  val set_operator_prefix :
       ?location:Location.t
    -> ?precedence:Int.t
    -> Qualified_identifier.t
    -> Unit.t t

  val set_operator_infix :
       ?location:Location.t
    -> ?precedence:Int.t
    -> ?associativity:Associativity.t
    -> Qualified_identifier.t
    -> Unit.t t

  val set_operator_postfix :
       ?location:Location.t
    -> ?precedence:Int.t
    -> Qualified_identifier.t
    -> Unit.t t

  val open_module : location:Location.t -> Qualified_identifier.t -> Unit.t t

  val add_module_abbreviation :
       ?location:Location.t
    -> Qualified_identifier.t
    -> abbreviation:Identifier.t
    -> Unit.t t

  val with_checkpoint : 'a t -> 'a t

  val next_module_id : Id.module_id t

  val index_lf_kind : Synext.lf_kind -> Synapx.LF.kind t

  val index_lf_typ : Synext.lf_typ -> Synapx.LF.typ t

  val index_schema : Synext.schema -> Synapx.LF.schema t

  val index_meta_context : Synext.meta_context -> Synapx.LF.mctx t

  val index_comp_kind : Synext.comp_kind -> Synapx.Comp.kind t

  val index_comp_typ : Synext.comp_typ -> Synapx.Comp.typ t

  val index_closed_comp_typ : Synext.comp_typ -> Synapx.Comp.typ t

  val index_comp_expression : Synext.comp_expression -> Synapx.Comp.exp t

  val index_comp_typedef :
       Synext.comp_typ
    -> Synext.comp_kind
    -> (Synapx.Comp.typ * Synapx.Comp.kind) t

  val index_comp_theorem : Synext.comp_expression -> Synapx.Comp.thm t

  val index_harpoon_proof : Synext.harpoon_proof -> Synapx.Comp.thm t

  val add_lf_type_constant :
    ?location:Location.t -> Identifier.t -> Id.cid_typ -> Unit.t t

  val add_lf_term_constant :
    ?location:Location.t -> Identifier.t -> Id.cid_term -> Unit.t t

  val add_schema_constant :
    ?location:Location.t -> Identifier.t -> Id.cid_schema -> Unit.t t

  val add_prog :
    ?location:Location.t -> Identifier.t -> Id.cid_prog -> Unit.t t

  val add_comp_val :
    ?location:Location.t -> Identifier.t -> Id.cid_prog -> Unit.t t

  val add_comp_typedef :
    ?location:Location.t -> Identifier.t -> Id.cid_comp_typdef -> Unit.t t

  val add_comp_inductive_type_constant :
    ?location:Location.t -> Identifier.t -> Id.cid_comp_typ -> Unit.t t

  val add_comp_stratified_type_constant :
    ?location:Location.t -> Identifier.t -> Id.cid_comp_typ -> Unit.t t

  val add_comp_cotype_constant :
    ?location:Location.t -> Identifier.t -> Id.cid_comp_cotyp -> Unit.t t

  val add_comp_constructor :
    ?location:Location.t -> Identifier.t -> Id.cid_comp_const -> Unit.t t

  val add_comp_destructor :
    ?location:Location.t -> Identifier.t -> Id.cid_comp_dest -> Unit.t t

  val add_module :
    ?location:Location.t -> Identifier.t -> Id.module_id -> 'a t -> 'a t
end

module Make_signature_reconstruction_state (Index_state : sig
  include Index_state.INDEXING_STATE

  val start_module : Unit.t t

  val stop_module :
    ?location:Location.t -> Identifier.t -> Id.module_id -> Unit.t t
end) =
struct
  type state =
    { leftover_vars : (Abstract.free_var Synint.LF.ctx * Location.t) List.t
    ; index_state : Index_state.state
    ; default_associativity : Associativity.t
    ; default_precedence : Int.t
    ; modules : Int.t
    }

  let initial_state index_state =
    { leftover_vars = []
    ; index_state
    ; default_associativity = Synext.default_associativity
    ; default_precedence = Synext.default_precedence
    ; modules = 0
    }

  include (
    State.Make (struct
      type t = state
    end) :
      State.STATE with type state := state)

  let get_leftover_vars =
    let* state = get in
    return state.leftover_vars

  let add_leftover_vars vars location =
    modify (fun state ->
        { state with
          leftover_vars = (vars, location) :: state.leftover_vars
        })

  let with_disabled_lf_strengthening ~location:_ m =
    let initial_strengthen = !Lfrecon.strengthen in
    fun state ->
      Lfrecon.strengthen := false;

      let state', x = run m state in

      Lfrecon.strengthen := initial_strengthen;

      (state', x)

  let with_warn_on_coverage_error ~location:_ m =
    let initial_enableCoverage, initial_warningOnly =
      (!Coverage.enableCoverage, !Coverage.warningOnly)
    in
    fun state ->
      Coverage.enableCoverage := true;
      Coverage.warningOnly := true;

      let state', x = run m state in

      Coverage.enableCoverage := initial_enableCoverage;
      Coverage.warningOnly := initial_warningOnly;

      (state', x)

  let with_coverage_checking ~location:_ m =
    let initial_enableCoverage = !Coverage.enableCoverage in
    fun state ->
      Coverage.enableCoverage := true;

      let state', x = run m state in

      Coverage.enableCoverage := initial_enableCoverage;

      (state', x)

  let[@inline] set_default_associativity ?location:_ default_associativity =
    modify (fun state -> { state with default_associativity })

  let get_default_associativity =
    let* state = get in
    return state.default_associativity

  let[@inline] set_default_precedence ?location:_ default_precedence =
    modify (fun state -> { state with default_precedence })

  let get_default_precedence =
    let* state = get in
    return state.default_precedence

  let get_index_state =
    let* state = get in
    return state.index_state

  let[@inline] set_index_state index_state =
    modify (fun state -> { state with index_state })

  let[@inline] modify_index_state f =
    let* index_state = get_index_state in
    let index_state' = f index_state in
    set_index_state index_state'

  let add_lf_type_constant ?location identifier cid =
    modify_index_state
      Index_state.(exec (add_lf_type_constant ?location identifier cid))

  let add_lf_term_constant ?location identifier cid =
    modify_index_state
      Index_state.(exec (add_lf_term_constant ?location identifier cid))

  let add_schema_constant ?location identifier cid =
    modify_index_state
      Index_state.(exec (add_schema_constant ?location identifier cid))

  let add_prog ?location identifier cid =
    modify_index_state
      Index_state.(exec (add_program_constant ?location identifier cid))

  let add_comp_val ?location identifier cid =
    modify_index_state
      Index_state.(exec (add_program_constant ?location identifier cid))

  let add_comp_typedef ?location identifier cid =
    modify_index_state
      Index_state.(
        exec
          (add_abbreviation_computation_type_constant ?location identifier
             cid))

  let add_comp_inductive_type_constant ?location identifier cid =
    modify_index_state
      Index_state.(
        exec
          (add_inductive_computation_type_constant ?location identifier cid))

  let add_comp_stratified_type_constant ?location identifier cid =
    modify_index_state
      Index_state.(
        exec
          (add_stratified_computation_type_constant ?location identifier cid))

  let add_comp_cotype_constant ?location identifier cid =
    modify_index_state
      Index_state.(
        exec
          (add_coinductive_computation_type_constant ?location identifier cid))

  let add_comp_constructor ?location identifier cid =
    modify_index_state
      Index_state.(
        exec (add_computation_term_constructor ?location identifier cid))

  let add_comp_destructor ?location identifier cid =
    modify_index_state
      Index_state.(
        exec (add_computation_term_destructor ?location identifier cid))

  let start_module = modify_index_state Index_state.(exec start_module)

  let stop_module ?location identifier cid =
    modify_index_state
      Index_state.(exec (stop_module ?location identifier cid))

  let add_module ?location identifier cid m =
    let* default_associativity = get_default_associativity in
    let* default_precedence = get_default_precedence in
    start_module &> m
    <& stop_module ?location identifier cid
    <& set_default_associativity default_associativity
    <& set_default_precedence default_precedence

  let next_module_id =
    let* state = get in
    let modules' = state.modules + 1 in
    let* () = put { state with modules = modules' } in
    return modules'

  (* This implementation is incorrect. {!type:state} may be mutable since its
     indexing state could be mutable. Furthermore, [m] could trigger
     side-effects in other modules.

     This function should never be used by the end user. Currently, it is
     only used during the reconstruction of the signature-level [--not]
     pragma, which itself is only used in tests. This pragma should be
     deprecated in favour of a test harness that explicitely checks for
     thrown exceptions. *)
  let with_checkpoint m =
    let* state = get in
    let* x = m in
    let* () = put state in
    return x

  let get_default_precedence_opt = function
    | Option.None -> get_default_precedence
    | Option.Some precedence -> return precedence

  let get_default_associativity_opt = function
    | Option.None -> get_default_associativity
    | Option.Some associativity -> return associativity

  let lookup_operator_arity =
    Obj.magic () (* TODO: Lookup constant ID, then lookup in the store *)

  let set_operator_prefix ?location ?precedence constant =
    let* precedence = get_default_precedence_opt precedence in
    lookup_operator_arity >>= function
    | Option.None ->
        Error.raise_at1_opt location (Unknown_constant_arity constant)
    | Option.Some arity ->
        if arity <> 1 then
          Error.raise_at1_opt location
            (Invalid_prefix_arity { constant; actual = arity })
        else
          let name = Name.make_from_qualified_identifier constant in
          Store.OpPragmas.addPragma name Fixity.prefix precedence
            Associativity.right_associative;
          return ()

  let set_operator_infix ?location ?precedence ?associativity constant =
    let* precedence = get_default_precedence_opt precedence in
    let* associativity = get_default_associativity_opt associativity in
    lookup_operator_arity >>= function
    | Option.None ->
        Error.raise_at1_opt location (Unknown_constant_arity constant)
    | Option.Some arity ->
        if arity <> 2 then
          Error.raise_at1_opt location
            (Invalid_infix_arity { constant; actual = arity })
        else
          let name = Name.make_from_qualified_identifier constant in
          Store.OpPragmas.addPragma name Fixity.infix precedence
            associativity;
          return ()

  let set_operator_postfix ?location ?precedence constant =
    let* precedence = get_default_precedence_opt precedence in
    lookup_operator_arity >>= function
    | Option.None ->
        Error.raise_at1_opt location (Unknown_constant_arity constant)
    | Option.Some arity ->
        if arity <> 1 then
          Error.raise_at1_opt location
            (Invalid_postfix_arity { constant; actual = arity })
        else
          let name = Name.make_from_qualified_identifier constant in
          Store.OpPragmas.addPragma name Fixity.postfix precedence
            Associativity.left_associative;
          return ()

  let add_module_abbreviation = Obj.magic () (* TODO: *)

  let set_name_generation_bases = Obj.magic () (* TODO: *)

  let open_module = Obj.magic () (* TODO: *)

  let index_lf_kind = Obj.magic () (* TODO: *)

  let index_lf_typ = Obj.magic () (* TODO: *)

  let index_schema = Obj.magic () (* TODO: *)

  let index_meta_context = Obj.magic () (* TODO: *)

  let index_comp_kind = Obj.magic () (* TODO: *)

  let index_closed_comp_typ = Obj.magic () (* TODO: *)

  let index_comp_typ = Obj.magic () (* TODO: *)

  let index_comp_expression = Obj.magic () (* TODO: *)

  let index_comp_typedef = Obj.magic () (* TODO: *)

  let index_comp_theorem = Obj.magic () (* TODO: *)

  let index_harpoon_proof = Obj.magic () (* TODO: *)
end