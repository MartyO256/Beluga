(** Disambiguation of the parser syntax to the external syntax.

    Elements of the syntax for Beluga require the symbol table for
    disambiguation. This module contains stateful functions for elaborating
    the context-free parser syntax to the data-dependent external syntax. The
    logic for the symbol lookups is repeated in the indexing phase to the
    approximate syntax.

    The "Beluga Language Specification" document contains additional details
    as to which syntactic structures have ambiguities. *)

open Support
open Beluga_syntax
open Disambiguation_state

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

exception Expected_lf_term_constant

exception Expected_lf_type_constant

(** {2 Exception Printing} *)

let () =
  Error.register_exception_printer (function
    | Illegal_identifier_lf_kind ->
        Format.dprintf "Identifiers may not appear as LF kinds."
    | Illegal_qualified_identifier_lf_kind ->
        Format.dprintf "Qualified identifiers may not appear as LF kinds."
    | Illegal_backward_arrow_lf_kind ->
        Format.dprintf "Backward arrows may not appear as LF kinds."
    | Illegal_hole_lf_kind ->
        Format.dprintf "Holes may not appear as LF kinds."
    | Illegal_lambda_lf_kind ->
        Format.dprintf "Lambdas may not appear as LF kinds."
    | Illegal_annotated_lf_kind ->
        Format.dprintf
          "Type ascriptions to terms may not appear as LF kinds."
    | Illegal_application_lf_kind ->
        Format.dprintf "Term applications may not appear as LF kinds."
    | Illegal_type_kind_lf_type ->
        Format.dprintf "The kind `type' may not appear as an LF type."
    | Illegal_hole_lf_type ->
        Format.dprintf "Holes may not appear as LF types."
    | Illegal_lambda_lf_type ->
        Format.dprintf "Lambdas may not appear as LF types."
    | Illegal_annotated_lf_type ->
        Format.dprintf "Type ascriptions may not appear as LF types."
    | Unbound_lf_type_constant identifier ->
        Format.dprintf "The LF type-level constant %a is unbound."
          Qualified_identifier.pp identifier
    | Illegal_type_kind_lf_term ->
        Format.dprintf "The kind `type' may not appear as an LF term."
    | Illegal_pi_lf_term ->
        Format.dprintf "Pi-kinds or types may not appear as LF terms."
    | Illegal_forward_arrow_lf_term ->
        Format.dprintf "Forward arrows may not appear as LF terms."
    | Illegal_backward_arrow_lf_term ->
        Format.dprintf "Backward arrows may not appear as LF terms."
    | Unbound_lf_term_constant identifier ->
        Format.dprintf "The LF term-level constant %a is unbound."
          Qualified_identifier.pp identifier
    | Expected_lf_term_constant ->
        Format.dprintf "Expected an LF term-level constant."
    | Expected_lf_type_constant ->
        Format.dprintf "Expected an LF type-level constant."
    | exn -> Error.raise_unsupported_exception_printing exn)

(** {1 Disambiguation} *)

module type LF_DISAMBIGUATION = sig
  (** @closed *)
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

  (** LF object definition for application disambiguation. *)
  module Lf_object = struct
    type t = Synprs.lf_object

    type location = Location.t

    let location = Synprs.location_of_lf_object
  end

  module Lf_application_disambiguation =
    Application_disambiguation.Make_application_disambiguation (Lf_object)

  (** [identify_operator_identifier expression identifier state] is
      [(state', primitive)] where [primitive] is the application
      disambiguation source expression corresponding to [expression].
      [primitive] is either a user-defined operator with [identifier], or a
      plain expression. *)
  let identify_operator_identifier expression identifier =
    lookup_operator identifier >>= function
    | Option.None ->
        return (Lf_application_disambiguation.make_expression expression)
    | Option.Some operator ->
        return
          (Lf_application_disambiguation.make_operator expression operator
             identifier)

  (** [identify_operator expression state] is [(state', primitive)] where
      [primitive] is the application disambiguation source expression
      corresponding to [expression]. [primitive] is either a user-defined
      operator, or a plain expression. *)
  let identify_operator expression =
    match expression with
    | Synprs.LF.Object.Raw_qualified_identifier
        { identifier; prefixed = false; _ } ->
        identify_operator_identifier expression identifier
    | Synprs.LF.Object.Raw_identifier { identifier; prefixed = false; _ } ->
        identify_operator_identifier expression
          (Qualified_identifier.make_simple identifier)
    | _ -> return (Lf_application_disambiguation.make_expression expression)

  (** [with_lf_variable_opt identifier_opt m state] is [m] run in the state
      locally derived from [state] with the addition of [identifier] as a
      bound LF variable if [identifier_opt = Option.Some identifier]. *)
  let[@inline] with_lf_variable_opt = function
    | Option.None -> Fun.id
    | Option.Some identifier -> with_lf_variable identifier

  (** [disambiguate_lf_kind object_ kind] is [(state', kind')] where [kind']
      is the LF kind disambiguated from [kind] in the disambiguation state
      [state].

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
        { location; parameter_identifier; parameter_sort; plicity; body } ->
        let* parameter_type' =
          traverse_option disambiguate_lf_typ parameter_sort
        in
        let* body' =
          with_lf_variable_opt parameter_identifier
            (disambiguate_lf_kind body)
        in
        return
          (Synext.LF.Kind.Pi
             { location
             ; parameter_identifier
             ; parameter_type = parameter_type'
             ; plicity
             ; body = body'
             })

  (** [disambiguate_lf_typ typ state] is [(state', typ')] where [typ'] is the
      LF type disambiguated from [typ] in the disambiguation state [state].

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
    | Synprs.LF.Object.Raw_identifier { location; identifier; _ } -> (
        (* As an LF type, plain identifiers are necessarily bound LF
           type-level constants. *)
        let qualified_identifier =
          Qualified_identifier.make_simple identifier
        in
        (* Lookup the identifier in the current state *)
        lookup_toplevel identifier >>= function
        | Result.Ok entry when Entry.is_lf_type_constant entry ->
            (* [identifier] appears as a bound LF type constant in the
               state *)
            return
              (Synext.LF.Typ.Constant
                 { location; identifier = qualified_identifier })
        | Result.Ok entry ->
            (* [identifier] appears as a bound entry in the state, but it is
               not an LF type constant *)
            Error.raise_at1 location
              (Error.composite_exception2 Expected_lf_type_constant
                 (actual_binding_exn qualified_identifier entry))
        | Result.Error (Unbound_identifier _) ->
            (* [identifier] is unbound in the state *)
            Error.raise_at1 location
              (Unbound_lf_type_constant qualified_identifier)
        | Result.Error cause -> Error.raise_at1 location cause)
    | Synprs.LF.Object.Raw_qualified_identifier { location; identifier; _ }
      -> (
        (* Qualified identifiers without namespaces were parsed as plain
           identifiers *)
        assert (List.length (Qualified_identifier.namespaces identifier) >= 1);
        (* As an LF type, identifiers of the form [<identifier>
           <dot-identifier>+] are necessarily bound LF type-level
           constants. *)
        lookup identifier >>= function
        | Result.Ok entry when Entry.is_lf_type_constant entry ->
            return (Synext.LF.Typ.Constant { location; identifier })
        | Result.Ok entry ->
            Error.raise_at1 location
              (Error.composite_exception2 Expected_lf_type_constant
                 (actual_binding_exn identifier entry))
        | Result.Error (Unbound_qualified_identifier _) ->
            Error.raise_at1 location (Unbound_lf_type_constant identifier)
        | Result.Error cause -> Error.raise_at1 location cause)
    | Synprs.LF.Object.Raw_arrow { location; domain; range; orientation } ->
        (* Forward and backward arrows are allowed. *)
        let* domain' = disambiguate_lf_typ domain in
        let* range' = disambiguate_lf_typ range in
        return
          (Synext.LF.Typ.Arrow
             { location; domain = domain'; range = range'; orientation })
    | Synprs.LF.Object.Raw_pi
        { location; parameter_identifier; parameter_sort; plicity; body } ->
        let* parameter_type' =
          traverse_option disambiguate_lf_typ parameter_sort
        in
        let* body' =
          with_lf_variable_opt parameter_identifier
            (disambiguate_lf_typ body)
        in
        return
          (Synext.LF.Typ.Pi
             { location
             ; parameter_identifier
             ; parameter_type = parameter_type'
             ; plicity
             ; body = body'
             })
    | Synprs.LF.Object.Raw_application { objects; location } ->
        let* applicand, arguments = disambiguate_lf_application objects in
        let* applicand' = disambiguate_lf_typ applicand in
        let* arguments' = traverse_list1 elaborate_lf_operand arguments in
        return
          (Synext.LF.Typ.Application
             { applicand = applicand'; arguments = arguments'; location })

  (** [disambiguate_lf_term term state] is [(state', term')] where [term'] is
      the LF term disambiguated from [term] in the disambiguation state
      [state].

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
    | Synprs.LF.Object.Raw_identifier { location; identifier; _ } -> (
        (* As an LF term, plain identifiers are either term-level constants
           or variables (bound or free). *)
        let qualified_identifier =
          Qualified_identifier.make_simple identifier
        in
        (* Lookup the identifier in the current state *)
        lookup_toplevel identifier >>= function
        | Result.Ok entry when Entry.is_lf_term_constant entry ->
            (* [identifier] appears as an LF term-level constant in the
               state *)
            return
              (Synext.LF.Term.Constant
                 { location; identifier = qualified_identifier })
        | Result.Ok entry when Entry.is_lf_variable entry ->
            (* [identifier] appears as an LF bound variable in the state *)
            return (Synext.LF.Term.Variable { location; identifier })
        | Result.Ok entry ->
            (* [identifier] appears as a bound entry that is not an LF
               term-level constant or variable in the state *)
            Error.raise_at1 location
              (Error.composite_exception2 Expected_lf_term_constant
                 (actual_binding_exn qualified_identifier entry))
        | Result.Error (Unbound_identifier _) ->
            (* [identifier] is unbound in the state, so it is a free
               variable *)
            let* () = add_free_lf_variable identifier in
            return (Synext.LF.Term.Variable { location; identifier })
        | Result.Error cause -> Error.raise_at1 location cause)
    | Synprs.LF.Object.Raw_qualified_identifier { location; identifier; _ }
      -> (
        (* Qualified identifiers without namespaces were parsed as plain
           identifiers *)
        assert (List.length (Qualified_identifier.namespaces identifier) >= 1);
        (* As an LF term, identifiers of the form [<identifier>
           <dot-identifier>+] are necessarily LF term-level constants. Plain
           LF terms do not support projections, so there is no additional
           ambiguity. *)
        (* Lookup the identifier in the current state *)
        lookup identifier >>= function
        | Result.Ok entry when Entry.is_lf_term_constant entry ->
            (* [identifier] appears as an LF term-level constant *)
            return (Synext.LF.Term.Constant { location; identifier })
        | Result.Ok entry ->
            (* [identifier] appears as a bound entry that is not an LF
               term-level constant *)
            Error.raise_at1 location
              (Error.composite_exception2 Expected_lf_term_constant
                 (actual_binding_exn identifier entry))
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
          with_lf_variable_opt parameter_identifier
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
          (Synext.LF.Term.Type_annotated
             { location; term = term'; typ = typ' })
    | Synprs.LF.Object.Raw_application { objects; location } ->
        let* applicand, arguments = disambiguate_lf_application objects in
        let* applicand' = disambiguate_lf_term applicand in
        let* arguments' = traverse_list1 elaborate_lf_operand arguments in
        return
          (Synext.LF.Term.Application
             { applicand = applicand'; arguments = arguments'; location })

  (** [disambiguate_lf_application objects state] is
      [(state', (applicand, arguments))] where [applicand] and [arguments]
      the topmost applicand and arguments to use for the application node.
      Arguments in [arguments] may be applications themselves. User-defined
      operators and term juxtapositions are supported. *)
  and disambiguate_lf_application objects =
    let* objects' = traverse_list2 identify_operator objects in
    return (Lf_application_disambiguation.disambiguate_application objects')

  (** [elaborate_lf_operand operand state] is [(state', operand')] where
      [operand'] is [operand] disambiguated as an LF term with respect to the
      disambiguation state [state]. *)
  and elaborate_lf_operand operand =
    match operand with
    | Lf_application_disambiguation.Atom { expression; _ } ->
        disambiguate_lf_term expression
    | Lf_application_disambiguation.Application
        { applicand; arguments; location } ->
        let* applicand' = disambiguate_lf_term applicand in
        let* arguments' = traverse_list1 elaborate_lf_operand arguments in
        return
          (Synext.LF.Term.Application
             { applicand = applicand'; arguments = arguments'; location })
end
