(** Disambiguation of the parser syntax to the external syntax.

    Elements of the syntax for Beluga requires the symbol table for
    disambiguation. This module contains stateful functions for elaborating
    the context-free parser syntax to the data-dependent external syntax. The
    logic for the symbol lookups is repeated in the indexing phase to the
    approximate syntax.

    The "Beluga Language Specification" document contains additional details
    as to which syntactic structures have ambiguities. *)

open Support
open Beluga_syntax
open Common_disambiguation

(** {1 Exceptions} *)

(** {2 Exceptions for LF kind disambiguation} *)

exception Illegal_identifier_lf_kind

exception Illegal_qualified_identifier_lf_kind

exception Illegal_backward_arrow_lf_kind

exception Illegal_hole_lf_kind

exception Illegal_lambda_lf_kind

exception Illegal_annotated_lf_kind

exception Illegal_application_lf_kind

(** {2 Exceptions for LF type disambiguation} *)

exception Illegal_type_kind_lf_type

exception Illegal_hole_lf_type

exception Illegal_lambda_lf_type

exception Illegal_annotated_lf_type

exception Unbound_lf_type_constant of Qualified_identifier.t

(** {2 Exceptions for LF term disambiguation} *)

exception Illegal_type_kind_lf_term

exception Illegal_pi_lf_term

exception Illegal_forward_arrow_lf_term

exception Illegal_backward_arrow_lf_term

exception Unbound_lf_term_constant of Qualified_identifier.t

(** {2 Exceptions for LF type-level and term-level application rewriting} *)

exception Expected_lf_term_constant

exception Expected_lf_type_constant

exception Misplaced_lf_operator

exception Ambiguous_lf_operator_placement of Qualified_identifier.t

exception
  Consecutive_applications_of_non_associative_lf_operators of
    Qualified_identifier.t

exception
  Lf_operator_arity_mismatch of
    { operator_identifier : Qualified_identifier.t
    ; expected_arguments_count : Int.t
    ; actual_arguments_count : Int.t
    }

(** {1 Disambiguation} *)

module type LF_DISAMBIGUATION = sig
  include State.STATE

  (** {1 Disambiguation} *)

  val disambiguate_lf_kind : Synprs.lf_object -> Synext.lf_kind t

  val disambiguate_lf_typ : Synprs.lf_object -> Synext.lf_typ t

  val disambiguate_lf_term : Synprs.lf_object -> Synext.lf_term t
end

(** Disambiguation of LF kinds, types and terms from the parser syntax to the
    external syntax.

    This disambiguation does not perform normalization nor validation. *)
module Make (Disambiguation_state : DISAMBIGUATION_STATE) :
  LF_DISAMBIGUATION with type state = Disambiguation_state.state = struct
  include Disambiguation_state

  (** {1 Disambiguation} *)

  module Lf_operand = struct
    type expression = Synprs.lf_object

    type t =
      | Atom of expression
      | Application of
          { applicand : expression
          ; arguments : t List1.t
          }

    let rec location operand =
      match operand with
      | Atom object_ -> Synprs.location_of_lf_object object_
      | Application { applicand; arguments } ->
          let applicand_location = Synprs.location_of_lf_object applicand in
          let arguments_location =
            Location.join_all1_contramap location arguments
          in
          Location.join applicand_location arguments_location
  end

  module Lf_operator = struct
    type associativity = Associativity.t

    type fixity = Fixity.t

    type operand = Lf_operand.t

    type t =
      { identifier : Qualified_identifier.t
      ; operator : Operator.t
      ; applicand : Synprs.lf_object
      }

    let[@inline] make ~identifier ~operator ~applicand =
      { identifier; operator; applicand }

    let[@inline] operator o = o.operator

    let[@inline] applicand o = o.applicand

    let[@inline] identifier o = o.identifier

    let arity = Fun.(operator >> Operator.arity)

    let precedence = Fun.(operator >> Operator.precedence)

    let fixity = Fun.(operator >> Operator.fixity)

    let associativity = Fun.(operator >> Operator.associativity)

    let location = Fun.(identifier >> Qualified_identifier.location)

    let write operator arguments =
      let applicand = applicand operator in
      let arguments = List1.unsafe_of_list arguments in
      Lf_operand.Application { applicand; arguments }

    include (
      (val Eq.contramap (module Qualified_identifier) identifier) :
        Eq.EQ with type t := t)
  end

  module Application_disambiguation_state = struct
    include Disambiguation_state

    type operator = Lf_operator.t

    type expression = Synprs.lf_object

    let guard_identifier_operator identifier expression =
      lookup identifier >>= function
      | Result.Ok (LF_type_constant { operator })
      | Result.Ok (LF_term_constant { operator }) ->
          if Operator.is_nullary operator then return Option.none
          else
            return
              (Option.some
                 (Lf_operator.make ~identifier ~operator
                    ~applicand:expression))
      | Result.Ok _
      | Result.Error (Unbound_identifier _) ->
          return Option.none
      | Result.Error cause ->
          Error.raise_at1 (Qualified_identifier.location identifier) cause

    let guard_operator expression =
      match expression with
      | Synprs.LF.Object.Raw_identifier { quoted; _ }
      | Synprs.LF.Object.Raw_qualified_identifier { quoted; _ }
        when quoted ->
          return Option.none
      | Synprs.LF.Object.Raw_identifier { identifier; _ } ->
          let identifier = Qualified_identifier.make_simple identifier in
          guard_identifier_operator identifier expression
      | Synprs.LF.Object.Raw_qualified_identifier { identifier; _ } ->
          guard_identifier_operator identifier expression
      | Synprs.LF.Object.Raw_type _
      | Synprs.LF.Object.Raw_hole _
      | Synprs.LF.Object.Raw_pi _
      | Synprs.LF.Object.Raw_lambda _
      | Synprs.LF.Object.Raw_arrow _
      | Synprs.LF.Object.Raw_annotated _
      | Synprs.LF.Object.Raw_application _ ->
          return Option.none
  end

  module Lf_application_disambiguation =
    Application_disambiguation.Make (Associativity) (Fixity) (Lf_operand)
      (Lf_operator)
      (Application_disambiguation_state)

  let with_lf_term_variable identifier =
    scoped
      ~set:(add_lf_term_variable identifier)
      ~unset:(pop_binding identifier)

  let with_lf_term_variable_opt identifier_opt =
    match identifier_opt with
    | Option.None -> Fun.id
    | Option.Some identifier -> with_lf_term_variable identifier

  (** [disambiguate_lf_kind object_ state] is [object_] rewritten as an LF
      kind with respect to the disambiguation context [state].

      This function imposes syntactic restrictions on [object_], but does not
      perform normalization nor validation. To see the syntactic restrictions
      from raw LF objects to LF kinds, see the Beluga language specification. *)
  let rec disambiguate_lf_kind object_ =
    match object_ with
    | Synprs.LF.Object.Raw_identifier { location; _ } ->
        Error.raise_at1 location Illegal_identifier_lf_kind
    | Synprs.LF.Object.Raw_qualified_identifier { location; _ } ->
        Error.raise_at1 location Illegal_qualified_identifier_lf_kind
    | Synprs.LF.Object.Raw_hole { location; _ } ->
        Error.raise_at1 location Illegal_hole_lf_kind
    | Synprs.LF.Object.Raw_lambda { location; _ } ->
        Error.raise_at1 location Illegal_lambda_lf_kind
    | Synprs.LF.Object.Raw_annotated { location; _ } ->
        Error.raise_at1 location Illegal_annotated_lf_kind
    | Synprs.LF.Object.Raw_application { location; _ } ->
        Error.raise_at1 location Illegal_application_lf_kind
    | Synprs.LF.Object.Raw_type { location } ->
        return (Synext.LF.Kind.Typ { location })
    | Synprs.LF.Object.Raw_arrow { location; domain; range; orientation }
      -> (
        match orientation with
        | `Backward ->
            Error.raise_at1 location Illegal_backward_arrow_lf_kind
        | `Forward ->
            let* domain' = disambiguate_lf_typ domain in
            let* range' = disambiguate_lf_kind range in
            return
              (Synext.LF.Kind.Arrow
                 { location; domain = domain'; range = range' }))
    | Synprs.LF.Object.Raw_pi
        { location; parameter_identifier; parameter_sort; body } ->
        let* parameter_type' =
          traverse_option disambiguate_lf_typ parameter_sort
        in
        let* body' =
          with_lf_term_variable_opt parameter_identifier
            (disambiguate_lf_kind body)
        in
        return
          (Synext.LF.Kind.Pi
             { location
             ; parameter_identifier
             ; parameter_type = parameter_type'
             ; body = body'
             })

  (** [disambiguate_lf_typ object_ state] is [object_] rewritten as an LF
      type with respect to the disambiguation context [state].

      Type applications are rewritten with {!disambiguate_lf_application}
      using Dijkstra's shunting yard algorithm.

      This function imposes syntactic restrictions on [object_], but does not
      perform normalization nor validation. To see the syntactic restrictions
      from LF objects to LF types, see the Beluga language specification. *)
  and disambiguate_lf_typ object_ =
    match object_ with
    | Synprs.LF.Object.Raw_type { location; _ } ->
        Error.raise_at1 location Illegal_type_kind_lf_type
    | Synprs.LF.Object.Raw_hole { location; _ } ->
        Error.raise_at1 location Illegal_hole_lf_type
    | Synprs.LF.Object.Raw_lambda { location; _ } ->
        Error.raise_at1 location Illegal_lambda_lf_type
    | Synprs.LF.Object.Raw_annotated { location; _ } ->
        Error.raise_at1 location Illegal_annotated_lf_type
    | Synprs.LF.Object.Raw_identifier { location; identifier; quoted } -> (
        (* As an LF type, plain identifiers are necessarily type-level
           constants. *)
        let qualified_identifier =
          Qualified_identifier.make_simple identifier
        in
        lookup_toplevel identifier >>= function
        | Result.Ok (LF_type_constant { operator }) ->
            return
              (Synext.LF.Typ.Constant
                 { location
                 ; identifier = qualified_identifier
                 ; operator
                 ; quoted
                 })
        | Result.Ok _entry ->
            Error.raise_at1 location Expected_lf_type_constant
        | Result.Error (Unbound_identifier _) ->
            Error.raise_at1 location
              (Unbound_lf_type_constant qualified_identifier)
        | Result.Error cause -> Error.raise_at1 location cause)
    | Synprs.LF.Object.Raw_qualified_identifier
        { location; identifier; quoted } -> (
        (* Qualified identifiers without modules were parsed as plain
           identifiers *)
        assert (List.length (Qualified_identifier.modules identifier) >= 1);
        (* As an LF type, identifiers of the form [(<identifier> `::')+
           <identifier>] are necessarily type-level constants. *)
        lookup identifier >>= function
        | Result.Ok (LF_type_constant { operator }) ->
            return
              (Synext.LF.Typ.Constant
                 { location; identifier; operator; quoted })
        | Result.Ok _entry ->
            Error.raise_at1 location Expected_lf_type_constant
        | Result.Error (Unbound_qualified_identifier _) ->
            Error.raise_at1 location (Unbound_lf_type_constant identifier)
        | Result.Error cause -> Error.raise_at1 location cause)
    | Synprs.LF.Object.Raw_arrow { location; domain; range; orientation } ->
        let* domain' = disambiguate_lf_typ domain in
        let* range' = disambiguate_lf_typ range in
        return
          (Synext.LF.Typ.Arrow
             { location; domain = domain'; range = range'; orientation })
    | Synprs.LF.Object.Raw_pi
        { location; parameter_identifier; parameter_sort; body } ->
        let* parameter_type' =
          traverse_option disambiguate_lf_typ parameter_sort
        in
        let* body' =
          with_lf_term_variable_opt parameter_identifier
            (disambiguate_lf_typ body)
        in
        return
          (Synext.LF.Typ.Pi
             { location
             ; parameter_identifier
             ; parameter_type = parameter_type'
             ; body = body'
             })
    | Synprs.LF.Object.Raw_application { objects; location } ->
        let* applicand, arguments = disambiguate_lf_application objects in
        let* applicand' = disambiguate_lf_typ applicand in
        let* arguments' = traverse_list1 elaborate_lf_operand arguments in
        return
          (Synext.LF.Typ.Application
             { applicand = applicand'; arguments = arguments'; location })

  (** [disambiguate_lf_term object_ state] is [object_] rewritten as an LF
      term with respect to the disambiguation context [state].

      Term applications are rewritten with {!disambiguate_lf_application}
      using Dijkstra's shunting yard algorithm.

      This function imposes syntactic restrictions on [object_], but does not
      perform normalization nor validation. To see the syntactic restrictions
      from LF objects to LF terms, see the Beluga language specification. *)
  and disambiguate_lf_term object_ =
    match object_ with
    | Synprs.LF.Object.Raw_type { location; _ } ->
        Error.raise_at1 location Illegal_type_kind_lf_term
    | Synprs.LF.Object.Raw_pi { location; _ } ->
        Error.raise_at1 location Illegal_pi_lf_term
    | Synprs.LF.Object.Raw_arrow { location; orientation = `Forward; _ } ->
        Error.raise_at1 location Illegal_forward_arrow_lf_term
    | Synprs.LF.Object.Raw_arrow { location; orientation = `Backward; _ } ->
        Error.raise_at1 location Illegal_backward_arrow_lf_term
    | Synprs.LF.Object.Raw_identifier { location; identifier; quoted } -> (
        (* As an LF term, plain identifiers are either term-level constants
           or variables (bound or free). *)
        let qualified_identifier =
          Qualified_identifier.make_simple identifier
        in
        (* Lookup the identifier in the current state *)
        lookup_toplevel identifier >>= function
        | Result.Ok (LF_term_constant { operator }) ->
            (* [identifier] appears as an LF term-level constant *)
            return
              (Synext.LF.Term.Constant
                 { location
                 ; identifier = qualified_identifier
                 ; operator
                 ; quoted
                 })
        | Result.Ok LF_term_variable ->
            (* [identifier] appears as an LF bound variable *)
            return (Synext.LF.Term.Variable { location; identifier })
        | Result.Ok _entry ->
            (* [identifier] appears as a bound entry that is not an LF
               term-level constant or variable *)
            Error.raise_at1 location Expected_lf_term_constant
        | Result.Error (Unbound_identifier _) ->
            (* [identifier] does not appear in the state, so it is a free
               variable *)
            return (Synext.LF.Term.Variable { location; identifier })
        | Result.Error cause -> Error.raise_at1 location cause)
    | Synprs.LF.Object.Raw_qualified_identifier
        { location; identifier; quoted } -> (
        (* Qualified identifiers without modules were parsed as plain
           identifiers *)
        assert (List.length (Qualified_identifier.modules identifier) >= 1);
        (* As an LF term, identifiers of the form [(<identifier> `::')+
           <identifier>] are necessarily term-level constants. *)
        (* Lookup the identifier in the current state *)
        lookup identifier >>= function
        | Result.Ok (LF_term_constant { operator }) ->
            (* [identifier] appears as an LF term-level constant *)
            return
              (Synext.LF.Term.Constant
                 { location; identifier; operator; quoted })
        | Result.Ok _entry ->
            (* [identifier] appears as a bound entry that is not an LF
               term-level constant *)
            Error.raise_at1 location Expected_lf_term_constant
        | Result.Error (Unbound_qualified_identifier _) ->
            (* [identifier] does not appear in the state, and constants must
               be bound *)
            Error.raise_at1 location (Unbound_lf_term_constant identifier)
        | Result.Error cause -> Error.raise_at1 location cause)
    | Synprs.LF.Object.Raw_lambda
        { location; parameter_identifier; parameter_sort; body } ->
        let* parameter_type' =
          traverse_option disambiguate_lf_typ parameter_sort
        in
        let* body' =
          with_lf_term_variable_opt parameter_identifier
            (disambiguate_lf_term body)
        in
        return
          (Synext.LF.Term.Abstraction
             { location
             ; parameter_identifier
             ; parameter_type = parameter_type'
             ; body = body'
             })
    | Synprs.LF.Object.Raw_hole { location } ->
        return (Synext.LF.Term.Wildcard { location })
    | Synprs.LF.Object.Raw_annotated { location; object_; sort } ->
        let* term' = disambiguate_lf_term object_ in
        let* typ' = disambiguate_lf_typ sort in
        return
          (Synext.LF.Term.TypeAnnotated
             { location; term = term'; typ = typ' })
    | Synprs.LF.Object.Raw_application { objects; location } ->
        let* applicand, arguments = disambiguate_lf_application objects in
        let* applicand' = disambiguate_lf_term applicand in
        let* arguments' = traverse_list1 elaborate_lf_operand arguments in
        return
          (Synext.LF.Term.Application
             { applicand = applicand'; arguments = arguments'; location })

  and disambiguate_lf_application objects =
    Lf_application_disambiguation.disambiguate_application objects
    >>= function
    | Result.Ok (applicand, arguments) -> return (applicand, arguments)
    | Result.Error
        (Lf_application_disambiguation.Ambiguous_operator_placement
          { left_operator; right_operator }) ->
        let left_operator_location = Lf_operator.location left_operator in
        let right_operator_location = Lf_operator.location right_operator in
        let identifier = Lf_operator.identifier left_operator in
        Error.raise_at2 left_operator_location right_operator_location
          (Ambiguous_lf_operator_placement identifier)
    | Result.Error
        (Lf_application_disambiguation.Arity_mismatch
          { operator; operator_arity; operands }) ->
        let operator_identifier = Lf_operator.identifier operator in
        let operator_location = Lf_operator.location operator in
        let expected_arguments_count = operator_arity in
        let operand_locations = List.map Lf_operand.location operands in
        let actual_arguments_count = List.length operands in
        Error.raise_at
          (List1.from operator_location operand_locations)
          (Lf_operator_arity_mismatch
             { operator_identifier
             ; expected_arguments_count
             ; actual_arguments_count
             })
    | Result.Error
        (Lf_application_disambiguation.Consecutive_non_associative_operators
          { left_operator; right_operator }) ->
        let operator_identifier = Lf_operator.identifier left_operator in
        let left_operator_location = Lf_operator.location left_operator in
        let right_operator_location = Lf_operator.location right_operator in
        Error.raise_at2 left_operator_location right_operator_location
          (Consecutive_applications_of_non_associative_lf_operators
             operator_identifier)
    | Result.Error
        (Lf_application_disambiguation.Misplaced_operator
          { operator; operands }) ->
        let operator_location = Lf_operator.location operator
        and operand_locations = List.map Lf_operand.location operands in
        Error.raise_at
          (List1.from operator_location operand_locations)
          Misplaced_lf_operator
    | Result.Error cause -> Error.raise cause

  and elaborate_lf_operand operand =
    match operand with
    | Lf_operand.Atom object_ -> disambiguate_lf_term object_
    | Lf_operand.Application { applicand; arguments } ->
        let* applicand' = disambiguate_lf_term applicand in
        let* arguments' = traverse_list1 elaborate_lf_operand arguments in
        let location =
          Location.join_all1_contramap Synext.location_of_lf_term
            (List1.cons applicand' arguments')
        in
        return
          (Synext.LF.Term.Application
             { applicand = applicand'; arguments = arguments'; location })
end

(** {2 Exception Printing} *)

let pp_exception ppf = function
  | Illegal_identifier_lf_kind ->
      Format.fprintf ppf "Identifiers may not appear as LF kinds."
  | Illegal_qualified_identifier_lf_kind ->
      Format.fprintf ppf "Qualified identifiers may not appear as LF kinds."
  | Illegal_backward_arrow_lf_kind ->
      Format.fprintf ppf "Backward arrows may not appear as LF kinds."
  | Illegal_hole_lf_kind ->
      Format.fprintf ppf "Holes may not appear as LF kinds."
  | Illegal_lambda_lf_kind ->
      Format.fprintf ppf "Lambdas may not appear as LF kinds."
  | Illegal_annotated_lf_kind ->
      Format.fprintf ppf
        "Type ascriptions to terms may not appear as LF kinds."
  | Illegal_application_lf_kind ->
      Format.fprintf ppf "Term applications may not appear as LF kinds."
  | Illegal_type_kind_lf_type ->
      Format.fprintf ppf "The kind `type' may not appear as LF types."
  | Illegal_hole_lf_type ->
      Format.fprintf ppf "Holes may not appear as LF types."
  | Illegal_lambda_lf_type ->
      Format.fprintf ppf "Lambdas may not appear as LF types."
  | Illegal_annotated_lf_type ->
      Format.fprintf ppf "Type ascriptions may not appear as LF types."
  | Unbound_lf_type_constant identifier ->
      Format.fprintf ppf "The LF type-level constant %a is unbound."
        Qualified_identifier.pp identifier
  | Illegal_type_kind_lf_term ->
      Format.fprintf ppf "The kind `type' may not appear as LF terms."
  | Illegal_pi_lf_term ->
      Format.fprintf ppf "Pi-kinds or types may not appear as LF terms."
  | Illegal_forward_arrow_lf_term ->
      Format.fprintf ppf "Forward arrows may not appear as LF terms."
  | Illegal_backward_arrow_lf_term ->
      Format.fprintf ppf "Backward arrows may not appear as LF terms."
  | Unbound_lf_term_constant identifier ->
      Format.fprintf ppf "The LF term-level constant %a is unbound."
        Qualified_identifier.pp identifier
  | Expected_lf_term_constant ->
      Format.fprintf ppf "Expected an LF term-level constant."
  | Expected_lf_type_constant ->
      Format.fprintf ppf "Expected an LF type-level constant."
  | Misplaced_lf_operator ->
      Format.fprintf ppf "Misplaced LF term-level or type-level operator."
  | Ambiguous_lf_operator_placement operator_identifier ->
      Format.fprintf ppf
        "Ambiguous occurrences of the LF term-level or type-level operator \
         %a."
        Qualified_identifier.pp operator_identifier
  | Consecutive_applications_of_non_associative_lf_operators
      operator_identifier ->
      Format.fprintf ppf
        "The consecutive application of the non-associative LF term-level \
         or type-level %a is illegal."
        Qualified_identifier.pp operator_identifier
  | Lf_operator_arity_mismatch
      { operator_identifier
      ; expected_arguments_count
      ; actual_arguments_count
      } ->
      Format.fprintf ppf "Operator %a expected %d arguments but got %d."
        Qualified_identifier.pp operator_identifier expected_arguments_count
        actual_arguments_count
  | _ ->
      Error.raise (Invalid_argument "[pp_exception] unsupported exception")

let () =
  Printexc.register_printer (fun exn ->
      try Option.some (Format.stringify pp_exception exn) with
      | Invalid_argument _ -> Option.none)
