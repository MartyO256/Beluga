(** The external syntax of Beluga. *)

open Support
open Common

(** ASTs constructed with the constructors in this module are not necessarily
    in normal form.

    These types are suited for pretty-printing and elaboration to the
    internal syntax. Note that this is a named representation without
    ambiguities. *)

(** {1 External LF Syntax} *)

(** The representation of LF kinds, types, and terms after parsing and
    data-dependent disambiguation.

    These types are only intended to be used in the definition of LF
    type-level or term-level constants. This is a weak, representational
    function space without case analysis or recursion.

    LF terms as defined below may contain free variables. During the
    abstraction phase of term reconstruction, implicit binders are introduced
    for those free variables.

    The grammar metavariable:

    - {m x} ranges over variables
    - {m c} ranges over term-level constants
    - {m a} ranges over type-level constants

    {math
      \begin{aligned}
      &\text{LF kinds} &K &\Coloneqq
      \mathsf{type}
      \mid \Pi x {:} A. K
      \mid A \to K\\

      &\text{LF types} &A, B &\Coloneqq
      a
      \mid \Pi x {:} A. B
      \mid A \to B
      \mid A \; M_1 \; M_2 \; \dots \; M_n\\

      &\text{LF terms} &M, N &\Coloneqq
      c
      \mid x
      \mid \_
      \mid \lambda x {:} A. M
      \mid M \; N_1 \; N_2 \; \dots \; N_n
      \mid M : A
      \end{aligned}
    } *)

module LF = struct
  (** External LF kinds. *)
  module rec Kind : sig
    type t =
      | Typ of { location : Location.t }
          (** [Typ { _ }] is the kind of simple types [type]. *)
      | Arrow of
          { location : Location.t
          ; domain : Typ.t
          ; range : Kind.t
          }
          (** [Arrow { domain; range; _ }] is the kind [domain -> range]. *)
      | Pi of
          { location : Location.t
          ; parameter_identifier : Identifier.t Option.t
          ; parameter_type : Typ.t Option.t
          ; plicity : Plicity.t
          ; body : Kind.t
          }
          (** - [Pi { parameter_identifier = Option.Some "x"; parameter_type = t; body; _ }]
                is the dependent product kind [{ x : t } body]. The variable
                ["x"] ranges over LF terms.

              - [Pi { parameter_identifier = Option.None; parameter_type = t; body; _ }]
                is the dependent product kind [{ _ : t } body]. *)
  end =
    Kind

  (** External LF types. *)
  and Typ : sig
    type t =
      | Constant of
          { location : Location.t
          ; identifier : Qualified_identifier.t
          }
          (** [Constant { identifier = "c"; _ }] is the type-level constant
              with qualified identifier ["c"]. *)
      | Application of
          { location : Location.t
          ; applicand : Typ.t
          ; arguments : Term.t List1.t
          }
          (** [Application { applicand; arguments; _ }] is the type-level
              application of [applicand] with [arguments].

              - If [applicand = Typ.Constant { operator; _ }] and
                [Operator.is_infix operator], then
                [List.length arguments = 2].
              - If [applicand = Typ.Constant { operator; _ }] and
                [Operator.is_postfix operator], then
                [List.length arguments = 1]. *)
      | Arrow of
          { location : Location.t
          ; domain : Typ.t
          ; range : Typ.t
          ; orientation : [ `Forward | `Backward ]
          }
          (** - [Arrow { domain; range; orientation = `Forward; _ }] is the
                type [domain -> range].
              - [Arrow { range; domain; orientation = `Backward; _ }] is the
                type [range <- domain]. *)
      | Pi of
          { location : Location.t
          ; parameter_identifier : Identifier.t Option.t
          ; parameter_type : Typ.t Option.t
          ; plicity : Plicity.t
          ; body : Typ.t
          }
          (** - [Pi { parameter_identifier = Option.Some "x"; parameter_type = t; body; _ }]
                is the dependent product type [{ x : t } body]. The variable
                ["x"] ranges over LF terms.

              - [Pi { parameter_identifier = Option.None; parameter_type = t; body; _ }]
                is the dependent product type [{ _ : t } body]. *)
  end =
    Typ

  (** External LF terms. *)
  and Term : sig
    type t =
      | Variable of
          { location : Location.t
          ; identifier : Identifier.t
          }
          (** [Variable { identifier = "x"; _ }] is the term-level variable
              with name ["x"]. It may be free or bound. *)
      | Constant of
          { location : Location.t
          ; identifier : Qualified_identifier.t
          }
          (** [Constant { identifier = "c"; _ }] is the term-level constant
              with qualified identifier ["c"]. *)
      | Application of
          { location : Location.t
          ; applicand : Term.t
          ; arguments : Term.t List1.t
          }
          (** [Application { applicand; arguments; _ }] is the term-level
              application of [applicand] with [arguments].

              - If [applicand = Term.Constant { operator; _ }] and
                [Operator.is_infix operator], then
                [List.length arguments = 2].
              - If [applicand = Term.Constant { operator; _ }] and
                [Operator.is_postfix operator], then
                [List.length arguments = 1]. *)
      | Abstraction of
          { location : Location.t
          ; parameter_identifier : Identifier.t Option.t
          ; parameter_type : Typ.t Option.t
          ; body : Term.t
          }
          (** - [Abstraction { parameter_identifier = Option.Some "x"; body; _ }]
                is the term [\x. body]. The variable ["x"] ranges over LF
                terms.

              - [Abstraction { parameter_identifier = Option.None; body; _ }]
                is the term [\_. body]. *)
      | Wildcard of { location : Location.t }
          (** [Wildcard { _ }] is the omission [_] of a fresh term-level
              variable. *)
      | Type_annotated of
          { location : Location.t
          ; term : Term.t
          ; typ : Typ.t
          }
          (** [Type_annotated { term = u; typ = t; _ }] is the term [u : t]. *)
  end =
    Term
end

(** {1 External Contextual LF Syntax} *)

(** The representation of contextual LF types, terms, and patterns after
    parsing and data-dependent disambiguation.

    The distinction between contextual LF objects and plain LF objects is
    that contextual LF objects may have substitutions, and may appear in
    patterns. Plain LF objects are only used in the definition of type-level
    or term-level LF constants.

    Contextual LF terms as defined below may contain free variables. During
    the abstraction phase of term reconstruction, implicit binders are
    introduced for those free variables.

    The grammar metavariable:

    - {m x} ranges over variables
    - {m c} ranges over term-level constants
    - {m a} ranges over type-level constants
    - {m s} ranges over substitutions
    - {m g} ranges over contexts
    - {m \mathsf{id}} ranges over identifiers
    - {m n} ranges over integers

    {math
      \begin{aligned}
      &\text{Contextual LF types} &A, B &\Coloneqq
      a
      \mid \Pi x {:} A. B
      \mid A \to B
      \mid A \; M_1 \; M_2 \; \dots \; M_n \\&&&
      \mid \mathsf{block} (x_1 : A_1, x_2 : A_2, \dots, x_n : A_n)\\

      &\text{Contextual LF terms} &M, N &\Coloneqq
      c
      \mid x
      \mid \#x
      \mid \$x
      \mid \_
      \mid \lambda x {:} A. M
      \mid M \; N_1 \; N_2 \; \dots \; N_n \\&&&
      \mid M : A
      \mid M[\sigma]
      \mid \;?
      \mid \;?\mathsf{id}
      \mid \langle M_1; M_2; \dots; M_n \rangle
      \mid M.n
      \mid M.\mathsf{id}\\

      &\text{Contextual LF substitutions} &\sigma &\Coloneqq
      \char`\^
      \mid \dots
      \mid \sigma, M
      \mid s[\sigma]\\

      &\text{Contextual LF contexts} &\Psi &\Coloneqq
      \char`\^
      \mid g
      \mid \Psi, x : A\\

      &\text{Contextual LF patterns} &M_p, N_p &\Coloneqq
      c
      \mid x
      \mid \#x
      \mid \$x
      \mid \_
      \mid \lambda x {:} A. M_p
      \mid M_p \; N_{p,1} \; N_{p,2} \; \dots \; N_{p_n} \\&&&
      \mid M_p : A
      \mid M_p[\sigma]
      \mid \langle M_{p,1}; M_{p,2}; \dots; M_{p,n} \rangle
      \mid M.n
      \mid M.\mathsf{id}\\

      &\text{Contextual LF substitution patterns} &\sigma_p &\Coloneqq
      \char`\^
      \mid \dots
      \mid \sigma_p, M_p
      \mid s[\sigma]\\

      &\text{Contextual LF context patterns} &\Psi_p &\Coloneqq
      \char`\^
      \mid g
      \mid \Psi_p, x : A
      \end{aligned}
    } *)

module CLF = struct
  (** External contextual LF types. *)
  module rec Typ : sig
    type t =
      | Constant of
          { location : Location.t
          ; identifier : Qualified_identifier.t
          }
          (** [Constant { identifier = "c"; _ }] is the type-level constant
              with qualified identifier ["c"]. *)
      | Application of
          { location : Location.t
          ; applicand : Typ.t
          ; arguments : Term.t List1.t
          }
          (** [Application { applicand; arguments; _ }] is the type-level
              application of [applicand] with [arguments].

              - If [applicand = Typ.Constant { operator; _ }] and
                [Operator.is_infix operator], then
                [List.length arguments = 2].
              - If [applicand = Typ.Constant { operator; _ }] and
                [Operator.is_postfix operator], then
                [List.length arguments = 1]. *)
      | Arrow of
          { location : Location.t
          ; domain : Typ.t
          ; range : Typ.t
          ; orientation : [ `Forward | `Backward ]
          }
          (** - [Arrow { domain; range; orientation = `Forward; _ }] is the
                type [domain -> range].
              - [Arrow { range; domain; orientation = `Backward; _ }] is the
                type [range <- domain]. *)
      | Pi of
          { location : Location.t
          ; parameter_identifier : Identifier.t Option.t
          ; parameter_type : Typ.t
          ; plicity : Plicity.t
          ; body : Typ.t
          }
          (** - [Pi { parameter_identifier = Option.Some "x"; parameter_type = t; body; _ }]
                is the dependent product type [{ x : t } body]. The variable
                ["x"] ranges over LF terms.

              - [Pi { parameter_identifier = Option.None; parameter_type = t; body; _ }]
                is the dependent product type [{ _ : t } body]. *)
      | Block of
          { location : Location.t
          ; elements :
              [ `Unnamed of Typ.t
              | `Record of (Identifier.t * Typ.t) List1.t
              ]
          }
          (** - [Block { elements = `Unnamed t; _ }] is the block type
                [block t].

              - [Block { elements = `Record \[("x1", t1); ("x2", t2); ...; ("xn", tn)\]; _ }]
                is the block type [block (x1 : t1, x2 : t2, ..., xn : tn)].
                This is a dependent sum type, or telescope, with [tj] being
                able to refer to ["xi"] when [i < j]. *)
  end =
    Typ

  (** External contextual LF terms. *)
  and Term : sig
    type t =
      | Variable of
          { location : Location.t
          ; identifier : Identifier.t
          }
          (** [Variable { identifier = "x"; _ }] is the term-level variable
              with name ["x"]. It may be free or bound. *)
      | Parameter_variable of
          { location : Location.t
          ; identifier : Identifier.t
          }
          (** [Parameter_variable { identifier = "#x"; _ }] is the term-level
              parameter variable with name ["#x"]. It may be free or bound. *)
      | Substitution_variable of
          { location : Location.t
          ; identifier : Identifier.t
          }
          (** [Substitution_variable { identifier = "$x"; _ }] is the
              term-level substitution variable with name ["$x"]. It may be
              free or bound. *)
      | Constant of
          { location : Location.t
          ; identifier : Qualified_identifier.t
          }
          (** [Constant { identifier = "c"; _ }] is the term-level constant
              with qualified identifier ["c"]. *)
      | Substitution of
          { location : Location.t
          ; term : Term.t
          ; substitution : Substitution.t
          }
          (** [Substitution { term; substitution; _ }] is the term
              [term\[substitution\]]. *)
      | Application of
          { location : Location.t
          ; applicand : Term.t
          ; arguments : Term.t List1.t
          }
          (** [Application { applicand; arguments; _ }] is the term-level
              application of [applicand] with [arguments].

              - If [applicand = Term.Constant { operator; _ }] and
                [Operator.is_infix operator], then
                [List.length arguments = 2].
              - If [applicand = Term.Constant { operator; _ }] and
                [Operator.is_postfix operator], then
                [List.length arguments = 1]. *)
      | Abstraction of
          { location : Location.t
          ; parameter_identifier : Identifier.t Option.t
          ; parameter_type : Typ.t Option.t
          ; body : Term.t
          }
          (** - [Abstraction { parameter_identifier = Option.Some "x"; body; _ }]
                is the term [\x. body].
              - [Abstraction { parameter_identifier = Option.None; body; _ }]
                is the term [\_. body]. *)
      | Hole of
          { location : Location.t
          ; variant :
              [ `Underscore | `Unlabelled | `Labelled of Identifier.t ]
          }
          (** [Hole { variant; _ }] is the omission of a term for
              reconstruction.

              - If [variant = `Underscore], then it is the hole [_].
              - If [variant = `Unlabelled], then it is the hole [?].
              - If [variant = `Labelled label], then it is the hole [?label]. *)
      | Tuple of
          { location : Location.t
          ; terms : Term.t List1.t
          }
          (** [Tuple { terms; _ }] is the tuple term [<t1; t2; ...; tn>] if
              [List1.to_list terms = \[t1; t2; ...; tn\]].

              This should not be confused with computational-level tuples.
              The type of a contextual LF term-level tuple is a block. *)
      | Projection of
          { location : Location.t
          ; term : Term.t
          ; projection :
              [ `By_identifier of Identifier.t | `By_position of Int.t ]
          }
          (** - [Projection { term = u; projection = `By_identifier x; _ }]
                is the term [u.x].
              - [Projection { term = u; projection = `By_position n; _ }] is
                the term [u.n]. *)
      | Type_annotated of
          { location : Location.t
          ; term : Term.t
          ; typ : Typ.t
          }
          (** [Type_annotated { term = u; typ = t; _ }] is the term [u : t]. *)

    (** External contextual LF term patterns. *)
    module rec Pattern : sig
      type t =
        | Variable of
            { location : Location.t
            ; identifier : Identifier.t
            }
            (** [Variable { identifier = "x"; _ }] is the term-level variable
                pattern ["x"]. *)
        | Parameter_variable of
            { location : Location.t
            ; identifier : Identifier.t
            }
            (** [Parameter_variable { identifier = "#x"; _ }] is the
                term-level parameter variable pattern with name ["#x"]. *)
        | Substitution_variable of
            { location : Location.t
            ; identifier : Identifier.t
            }
            (** [Substitution_variable { identifier = "$x"; _ }] is the
                term-level substitution variable pattern with name ["$x"]. *)
        | Constant of
            { location : Location.t
            ; identifier : Qualified_identifier.t
            }
            (** [Constant { identifier = "c"; _ }] is the term-level constant
                pattern ["c"]. *)
        | Wildcard of { location : Location.t }
            (** [Wildcard _] is the term-level catch-all pattern [_]. *)
        | Tuple of
            { location : Location.t
            ; terms : Term.Pattern.t List1.t
            }
            (** [Tuple { terms; _ }] is the tuple pattern [<p1; p2; ...; pn>]
                if [terms = \[p1; p2; ...; pn\]]. *)
        | Projection of
            { location : Location.t
            ; term : Term.Pattern.t
            ; projection :
                [ `By_identifier of Identifier.t | `By_position of Int.t ]
            }
            (** [Projection { term; _ }] is the pattern for the projection of
                a tuple [term]. This projection is used to indicate which
                element of a tuple the scrutinee is matched against in a
                [case]-expression. *)
        | Abstraction of
            { location : Location.t
            ; parameter_identifier : Identifier.t Option.t
            ; parameter_type : Typ.t Option.t
            ; body : Term.Pattern.t
            }
            (** [Abstraction { parameter_identifier = Option.Some "x"; parameter_type = Option.Some t; body; _ }]
                is the pattern [\x:t. body]. *)
        | Substitution of
            { location : Location.t
            ; term : Term.Pattern.t
            ; substitution : Substitution.t
            }
            (** [Substitution { term; substitution; _ }] is the pattern
                [term\[substitution\]]. *)
        | Application of
            { location : Location.t
            ; applicand : Term.Pattern.t
            ; arguments : Term.Pattern.t List1.t
            }
            (** [Application { applicand; arguments; _ }] is the term-level
                application pattern of [applicand] with [arguments].

                - If [applicand = Term.Constant { operator; _ }] and
                  [Operator.is_infix operator], then
                  [List.length arguments = 2].
                - If [applicand = Term.Constant { operator; _ }] and
                  [Operator.is_postfix operator], then
                  [List.length arguments = 1]. *)
        | Type_annotated of
            { location : Location.t
            ; term : Term.Pattern.t
            ; typ : Typ.t
            }
            (** [Type_annotated { term = x; typ = t; _ }] is the pattern
                [x : t]. *)
    end
  end =
    Term

  (** External contextual LF substitutions. *)
  and Substitution : sig
    (** [{ Substitution.head; terms; _ }] is the substitution

        - [^] if [head = Head.None] and [terms = \[\]]
        - [m1, m2, ..., mn] if [head = Head.None] and
          [terms = \[m1; m2; ...; mn\]]
        - [..] if [head = Head.Identity _] and [terms = \[\]]
        - [.., m1, m2, ..., mn] if [head = Head.Identitiy _] and
          [terms = \[m1; m2; ...; mn\]]
        - [$S] if
          [head = Head.Substitution_variable { identifier = "$S"; closure = Option.None; _ }]
          and [terms = \[\]]
        - [$S\[o\]] if
          [head = Head.Substitution_variable { identifier = "$S"; closure = Option.Some o; _ }]
          and [terms = \[\]]
        - [$S, m1, m2, ..., mn] if
          [head = Head.Substitution_variable { identifier = "$S"; closure = Option.None; _ }]
          and [terms = \[m1; m2; ...; mn\]]
        - [$S\[o\], m1, m2, ..., mn] if
          [head = Head.Substitution_variable { identifier = "$S"; closure = Option.Some o; _ }]
          and [terms = \[m1; m2; ...; mn\]] *)
    type t =
      { location : Location.t
      ; head : Substitution.Head.t
      ; terms : Term.t List.t
      }

    module Head : sig
      type t =
        | None of { location : Location.t }
        | Identity of { location : Location.t }
        | Substitution_variable of
            { location : Location.t
            ; identifier : Identifier.t
            ; closure : Substitution.t Option.t
            }
    end

    (** External contextual LF substitution patterns. *)
    module Pattern : sig
      type t =
        { location : Location.t
        ; head : Substitution.Pattern.Head.t
        ; terms : Term.Pattern.t List.t
        }

      module Head : sig
        type t =
          | None of { location : Location.t }
          | Identity of { location : Location.t }
          | Substitution_variable of
              { location : Location.t
              ; identifier : Identifier.t
              ; closure : Substitution.t Option.t
              }
      end
    end
  end =
    Substitution

  (** External contextual LF contexts. *)
  and Context : sig
    (** [{ Context.head; bindings; _ }] is the context

        - [^] if [head = Head.None] and [bindings = \[\]].
        - [x1 : t1, x2 : t2, ..., xn : tn] if [head = Head.None] and
          [bindings = \[("x1", t1); ("x2", t2); ..., ("xn", tn)\]].
        - [_] if [head = Head.Hole] and [bindings = \[\]].
        - [_, x1 : t1, x2 : t2, ..., xn : tn] if [head = Head.Hole] and
          [bindings = \[("x1", t1); ("x2", t2); ..., ("xn", tn)\]].
        - [g] if [head = Head.Context_variable { identifier = "g"; _ }] and
          [bindings = \[\]].
        - [g, x1 : t1, x2 : t2, ..., xn : tn] if
          [head = Head.Context_variable { identifier = "g"; _ }] and
          [bindings = \[("x1", t1); ("x2", t2); ..., ("xn", tn)\]]. *)
    type t =
      { location : Location.t
      ; head : Context.Head.t
      ; bindings : (Identifier.t * Typ.t Option.t) List.t
      }

    module Head : sig
      type t =
        | None of { location : Location.t }
        | Hole of { location : Location.t }
        | Context_variable of
            { location : Location.t
            ; identifier : Identifier.t
            }
    end

    (** External contextual LF context patterns. *)
    module Pattern : sig
      type t =
        { location : Location.t
        ; head : Context.Pattern.Head.t
        ; bindings : (Identifier.t * Typ.t) List.t
        }

      module Head : sig
        type t =
          | None of { location : Location.t }
          | Hole of { location : Location.t }
          | Context_variable of
              { location : Location.t
              ; identifier : Identifier.t
              }
      end
    end
  end =
    Context
end

(** {1 External Meta-Syntax} *)

(** The representation of meta-types, meta-objects, meta-object patterns,
    meta-substitutions and meta-contexts after parsing and data-dependent
    disambiguation.

    The grammar metavariable:

    - {m X} ranges over meta-level variables
    - {m g} ranges over context schemas

    {math
      \begin{aligned}
      &\text{Meta-types} &U &\Coloneqq
      g
      \mid [\Psi \vdash A]
      \mid \#[\Psi \vdash A]
      \mid \$[\Psi ⊢ \Psi]
      \mid \$[\Psi \vdash\!\!\#\; \Psi]\\

      &\text{Meta-objects} &C &\Coloneqq
      [\Psi]
      \mid [\Psi \vdash M]
      \mid \$[\Psi \vdash \sigma]
      \mid \$[\Psi \vdash\!\!\#\; \sigma]\\

      &\text{Meta-substitutions} &\theta &\Coloneqq
      \char`\^
      \mid \theta, C / X\\

      &\text{Meta-contexts} &\Delta &\Coloneqq
      \char`\^
      \mid g
      \mid \Delta, X : U\\

      &\text{Schemas} &G &\Coloneqq
      g
      \mid G + G
      \mid \mathsf{some} [x_1 : A_1, x_2 : A_2, \dots, x_n : A_n] \;
      \mathsf{block} (y_1 : B_1, y_2 : B_2, \dots, y_m : B_m)\\

      &\text{Meta-object patterns} &C_p &\Coloneqq
      [\Psi_p]
      \mid [\Psi_p \vdash M_p]
      \mid \$[\Psi_p \vdash \sigma_p]
      \mid \$[\Psi_p \vdash\!\!\#\; \sigma_p]
      \end{aligned}
    } *)

module Meta = struct
  (** External meta-types. *)
  module rec Typ : sig
    type t =
      | Context_schema of
          { location : Location.t
          ; schema : Schema.t
          }
          (** [Context_schema_constant { schema; _ }] is the context schema
              [schema]. *)
      | Contextual_typ of
          { location : Location.t
          ; context : CLF.Context.t
          ; typ : CLF.Typ.t
          }
          (** [Contextual_typ { context; typ; _ }] is the contextual type
              [(context |- typ)]. *)
      | Parameter_typ of
          { location : Location.t
          ; context : CLF.Context.t
          ; typ : CLF.Typ.t
          }
          (** [Parameter_typ { context; typ; _ }] is the parameter type
              [#(context |- typ)]. *)
      | Plain_substitution_typ of
          { location : Location.t
          ; domain : CLF.Context.t
          ; range : CLF.Context.t
          }
          (** [Plain_substitution_typ { domain; range; _ }] is the type for
              the plain substitution [$(domain |- range)]. *)
      | Renaming_substitution_typ of
          { location : Location.t
          ; domain : CLF.Context.t
          ; range : CLF.Context.t
          }
          (** [Renaming_substitution_typ { domain; range; _ }] is the type
              for the renaming substitution [$(domain |-# range)]. *)
  end =
    Typ

  (** External meta-objects. *)
  and Object : sig
    type t =
      | Context of
          { location : Location.t
          ; context : CLF.Context.t
          }
          (** [Context { context; _ }] is the context meta-object
              [\[context\]]. *)
      | Contextual_term of
          { location : Location.t
          ; context : CLF.Context.t
          ; term : CLF.Term.t
          }
          (** [Contextual_term { context; term; _ }] is the contextual term
              [\[context |- term\]]. *)
      | Plain_substitution of
          { location : Location.t
          ; domain : CLF.Context.t
          ; range : CLF.Substitution.t
          }
          (** [Plain_substitution { domain; range; _ }] is the plain
              substitution [$\[domain |- range\]]. *)
      | Renaming_substitution of
          { location : Location.t
          ; domain : CLF.Context.t
          ; range : CLF.Substitution.t
          }
          (** [Renaming_substitution { domain; range; _ }] is the renaming
              substitution [$\[domain |-# range\]]. *)
  end =
    Object

  (** External meta-object patterns. *)
  and Pattern : sig
    type t =
      | Context of
          { location : Location.t
          ; context : CLF.Context.Pattern.t
          }
          (** [Context { context; _ }] is the context meta-object pattern
              [\[context\]]. *)
      | Contextual_term of
          { location : Location.t
          ; context : CLF.Context.Pattern.t
          ; term : CLF.Term.Pattern.t
          }
          (** [Contextual_term { context; term; _ }] is the contextual term
              pattern [\[context |- term\]]. *)
      | Plain_substitution of
          { location : Location.t
          ; domain : CLF.Context.Pattern.t
          ; range : CLF.Substitution.Pattern.t
          }
          (** [Plain_substitution { domain; range; _ }] is the plain
              substitution pattern [$\[domain |- range\]]. *)
      | Renaming_substitution of
          { location : Location.t
          ; domain : CLF.Context.Pattern.t
          ; range : CLF.Substitution.Pattern.t
          }
          (** [Renaming_substitution { domain; range; _ }] is the renaming
              substitution pattern [$\[domain #|- range\]]. *)
  end =
    Pattern

  (** External meta-contexts. *)
  and Context : sig
    (** [{ Context.bindings; _ }] is the meta-context

        - [^] if [bindings = \[\]]
        - [x1 : a1, x2 : a2, ..., xn : an] if
          [bindings = \[("x1", a1, _); ("x2", a2, _); ...; ("xn", an, _)\]] *)
    type t =
      { location : Location.t
      ; bindings : (Identifier.t * Typ.t) List.t
      }
  end =
    Context

  (** External context schemas. *)
  and Schema : sig
    type t =
      | Constant of
          { location : Location.t
          ; identifier : Qualified_identifier.t
          }
          (** [Constant { identifier = "ctx"; _ }] is the schema having
              identifier ["ctx"] declared elsewhere in the signature.

              A tuple term has a block type [t] matching against this schema
              if [t] matches against the schema referred to as `identifier'. *)
      | Alternation of
          { location : Location.t
          ; schemas : Schema.t List2.t
          }
          (** [Alternation { schemas = \[g1; g2; ...; gn\]; _ }] is the
              schema [g1 + g2 + ... + gn].

              A tuple term has a block type [t] matching against this schema
              if [t] matches against at least one of [g1], [g2], ..., [gn]. *)
      | Element of
          { location : Location.t
          ; some : (Identifier.t * CLF.Typ.t) List1.t Option.t
          ; block :
              [ `Unnamed of CLF.Typ.t
              | `Record of (Identifier.t * CLF.Typ.t) List1.t
              ]
          }
          (** - [Element { some = \[("x1", p1); ("x2", p2); ...; ("xn", pn)\]; block = `Unnamed t; _ }]
                is the schema
                [some \[x1 : p1, x2 : p2, ..., xn : pn\] block t].

              - [Element { some = \[("x1", p1); ("x2", p2); ...; ("xn", pn)\]; block = `Record \[("y1", q1); ("y2", q2); ...; ("yn", qn)\]; _ }]
                is the schema
                [some \[x1 : p1, x2 : p2, ..., xn : pn\] block (y1 : q1, y2 : q2, ..., yn : qn)].

              A tuple term has a block type [t] matching against this schema
              if there exist terms having types in [p] in the context, and if
              the elements in [t] match against those in [q]. *)
  end =
    Schema
end

(** {1 External Computational Syntax} *)

(** The representation of computation-level kinds, types, expressions and
    patterns after parsing and data-dependent disambiguation.

    The grammar metavariable:

    - {m x} ranges over computation-level variables
    - {m c} ranges over computation-level constants
    - {m X} ranges over meta-level variables

    {math
      \begin{aligned}
      &\text{Computational kinds} &K &\Coloneqq
      \mathsf{ctype}
      \mid \Pi X {:} U. K
      \mid U \to K\\

      &\text{Computational types} &T, S &\Coloneqq
      \Pi X {:} U. S
      \mid T \to S
      \mid T_1 \times T_2 \times \dots \times T_n
      \mid U \\&&&
      \mid T \; C_1 \; C_2 \; \dots \; C_n\\

      &\text{Computational expressions} &E &\Coloneqq
      x
      \mid c
      \mid \mathsf{let} \; x = P \; \mathsf{in} \; E
      \mid \mathsf{impossible} \; E \\&&&
      \mid (E_1, E_2, \dots, E_n)
      \mid \;?
      \mid \;?\mathsf{id}
      \mid \_
      \mid E \; E_1 \; E_2 \; \dots \; E_n \\&&&
      \mid \mathsf{case} \; E \; \mathsf{of} \; P_1 \Rightarrow E_1 | P_2 \Rightarrow E_2 | \dots | P_n \Rightarrow E_n \\&&&
      \mid \mathsf{fn} \; x_1 \; x_2 \; \dots \; x_n \Rightarrow E
      \mid \mathsf{mlam} \; X_1 \; X_2 \; \dots \; X_n \Rightarrow E \\&&&
      \mid \mathsf{fun} \; P_1 \Rightarrow E_1 | P_2 \Rightarrow E_2 | \dots | P_n \Rightarrow E_n\\&&&
      \mid e .c\\

      &\text{Computational patterns} &P &\Coloneqq
      x
      \mid c
      \mid C_p
      \mid (P_1, P_2, \dots, P_n)
      \mid P \; P_1 \; P_2 \; \dots \; P_n\\

      &\text{Computational context} &\Gamma &\Coloneqq
      \char`\^
      \mid \Gamma, x : T
      \end{aligned}
    } *)

module Comp = struct
  (** External computation-level kinds. *)
  module rec Kind : sig
    type t =
      | Ctype of { location : Location.t }
          (** [Ctype { _ }] is the kind of computational types [ctype]. *)
      | Arrow of
          { location : Location.t
          ; domain : Meta.Typ.t
          ; range : Kind.t
          }
          (** [Arrow { domain; range; _ }] is the computational kind
              [domain -> range]. *)
      | Pi of
          { location : Location.t
          ; parameter_identifier : Identifier.t Option.t
          ; plicity : Plicity.t
          ; parameter_type : Meta.Typ.t
          ; body : Kind.t
          }
          (** [Pi { parameter_identifier = Option.Some "x"; parameter_type = t; explicit; body; _ }]
              is the explicit dependent product kind [{ x : t } body] if
              [plicity = Plicity.Explicit], and the implicit dependent
              product kind [(x : t) body] if [plicity = Plicity.Implicit].
              The variable ["x"] ranges over computational terms. *)
  end =
    Kind

  (** External computation-level types. *)
  and Typ : sig
    type t =
      | Inductive_typ_constant of
          { location : Location.t
          ; identifier : Qualified_identifier.t
          }
          (** [Inductive_typ_constant { identifier = "c"; }] is the
              computation-level inductive type constant ["c"]. *)
      | Stratified_typ_constant of
          { location : Location.t
          ; identifier : Qualified_identifier.t
          }
          (** [Stratified_typ_constant { identifier = "c"; }] is the
              computation-level stratified type constant ["c"]. *)
      | Coinductive_typ_constant of
          { location : Location.t
          ; identifier : Qualified_identifier.t
          }
          (** [Coinductive_typ_constant { identifier = "c"; }] is the
              computation-level coinductive type constant ["c"]. *)
      | Abbreviation_typ_constant of
          { location : Location.t
          ; identifier : Qualified_identifier.t
          }
          (** [Abbreviation_typ_constant { identifier = "c"; }] is the
              computation-level abbreviated type constant ["c"]. *)
      | Pi of
          { location : Location.t
          ; parameter_identifier : Identifier.t Option.t
          ; plicity : Plicity.t
          ; parameter_type : Meta.Typ.t
          ; body : Typ.t
          }
          (** [Pi { parameter_identifier = Option.Some "x"; parameter_type = t; explicit; body; _ }]
              is the explicit dependent product type [{ x : t } body] if
              [plicity = Plicity.Explicit], and the implicit dependent
              product type [(x : t) body] if [plicity = Plicity.Implicit].
              The variable ["x"] ranges over computational terms. *)
      | Arrow of
          { location : Location.t
          ; domain : Typ.t
          ; range : Typ.t
          ; orientation : [ `Forward | `Backward ]
          }
          (** [Arrow { domain; range; _ }] is the computational type
              [domain -> range]. *)
      | Cross of
          { location : Location.t
          ; types : Typ.t List2.t
          }
          (** [Cross { typs = \[t1; t2; ...; tn\]; _ }] is the type of tuple
              [t1 * t2 * ... * tn]. *)
      | Box of
          { location : Location.t
          ; meta_type : Meta.Typ.t
          }  (** [Box { typ = u; _ }] is a boxed meta-type [\[u\]]. *)
      | Application of
          { location : Location.t
          ; applicand : Typ.t
          ; arguments : Meta.Object.t List1.t
          }
          (** [Application { applicand; arguments; _ }] is the application of
              [applicand] with [arguments]. *)
  end =
    Typ

  (** External computation-level expressions. *)
  and Expression : sig
    type t =
      | Variable of
          { location : Location.t
          ; identifier : Identifier.t
          }
          (** [Variable { identifier = "x"; _ }] is the computation-level
              variable ["x"]. *)
      | Constructor of
          { location : Location.t
          ; identifier : Qualified_identifier.t
          }
          (** [Constructor { identifier = "c"; _ }] is the computation-level
              constant ["c"] referring to a constructor. *)
      | Program of
          { location : Location.t
          ; identifier : Qualified_identifier.t
          }
          (** [Program { identifier = "f"; _ }] is the computation-level
              constant ["f"] referring to a value. *)
      | Fn of
          { location : Location.t
          ; parameters : Identifier.t Option.t List1.t
          ; body : Expression.t
          }
          (** [Fn { parameters = \["x1"; "x2"; ...; "xn"\]; body; _ }] is the
              computation-level abstraction [fn x1, x2, ..., xn => body]. *)
      | Mlam of
          { location : Location.t
          ; parameters :
              (Identifier.t Option.t * [ `Plain | `Hash | `Dollar ]) List1.t
          ; body : Expression.t
          }
          (** [Mlam { parameters = \["X1"; "X2"; ...; "Xn"\]; body; _ }] is
              the computation-level abstraction over meta-objects
              [mlam X1, X2, ..., Xn => body]. *)
      | Fun of
          { location : Location.t
          ; branches : Cofunction_branch.t List1.t
          }
          (** [Fun { branches = \[(p1s, e1); (p2, e2); ...; (pn, en)\]; _ }]
              is the pattern-matching computation-level abstraction
              [fun p1 => e1 | p2 => e2 | ... | pn => en]. Each of [p1], [p2],
              ..., [pn] is a non-empty list of comma-separated copatterns,
              making this a cofunction. *)
      | Let of
          { location : Location.t
          ; meta_context : Meta.Context.t
          ; pattern : Pattern.t
          ; scrutinee : Expression.t
          ; body : Expression.t
          }
          (** [Let { pattern = p; scrutinee = e1; body = e2; _ }] is the
              pattern-matching let-binding expression [let p = e1 in e2]. *)
      | Box of
          { location : Location.t
          ; meta_object : Meta.Object.t
          }
          (** [Box { meta_object = C; _ }] is the meta-object expression
              [\[C\]]. *)
      | Impossible of
          { location : Location.t
          ; scrutinee : Expression.t
          }
          (** [Impossible { expression = e; _ }] is the pattern-matching with
              no branches [impossible e]. *)
      | Case of
          { location : Location.t
          ; scrutinee : Expression.t
          ; check_coverage : Bool.t
          ; branches : Case_branch.t List1.t
          }
          (** [Case { scrutinee = e; branches = \[(p1, e1); (p2, e2); ...; (pn, en)\]; check_coverage; _ }]
              is the pattern-matching expression
              [case e of p1 => e1 | p2 => e2 | ... | pn => en].

              If [check_coverage = false], then coverage-checking is disabled
              for this case-expression. *)
      | Tuple of
          { location : Location.t
          ; elements : Expression.t List2.t
          }
          (** [Tuple { elements = \[e1; e2; ...; en\]; _ }] is the tuple
              expression [(e1, e2, ..., en)]. *)
      | Hole of
          { location : Location.t
          ; label : Identifier.t Option.t
          }
          (** [Hole { label = Option.Some "x"; _ }] is the hole [?x] ranging
              over computatiion-level expressions. *)
      | Box_hole of { location : Location.t }
          (** [Box_hole _] is the hole [_] ranging over meta-objects. *)
      | Application of
          { location : Location.t
          ; applicand : Expression.t
          ; arguments : Expression.t List1.t
          }
          (** [Application { applicand; arguments; _ }] the application of
              [applicand] with [arguments]. *)
      | Observation of
          { location : Location.t
          ; scrutinee : Expression.t
          ; destructor : Qualified_identifier.t
          }
          (** [Observation { scrutinee = e; destructor = "c"; _ }] is the
              observation [e.c]. *)
      | Type_annotated of
          { location : Location.t
          ; expression : Expression.t
          ; typ : Typ.t
          }
          (** [Type_annotated { expression = e; typ = t; _ }] is the
              type-annotated computation-level expression [e : t]. *)
  end =
    Expression

  (** External case analysis branches. *)
  and Case_branch : sig
    type t =
      { location : Location.t
      ; meta_context : Meta.Context.t
      ; pattern : Pattern.t
      ; body : Expression.t
      }
  end =
    Case_branch

  (** External cofunction branches. *)
  and Cofunction_branch : sig
    type t =
      { location : Location.t
      ; meta_context : Meta.Context.t
      ; copattern : Copattern.t
      ; body : Expression.t
      }
  end =
    Cofunction_branch

  (** External computation-level patterns. *)
  and Pattern : sig
    type t =
      | Variable of
          { location : Location.t
          ; identifier : Identifier.t
          }
      | Constant of
          { location : Location.t
          ; identifier : Qualified_identifier.t
          }
      | Meta_object of
          { location : Location.t
          ; meta_pattern : Meta.Pattern.t
          }
      | Tuple of
          { location : Location.t
          ; elements : Pattern.t List2.t
          }
      | Application of
          { location : Location.t
          ; applicand : Pattern.t
          ; arguments : Pattern.t List1.t
          }
      | Type_annotated of
          { location : Location.t
          ; pattern : Pattern.t
          ; typ : Typ.t
          }
      | Wildcard of { location : Location.t }
  end =
    Pattern

  (** External computation-level copatterns. *)
  and Copattern : sig
    type t =
      { location : Location.t
      ; patterns : Pattern.t List.t
      ; observations : (Qualified_identifier.t * Pattern.t List.t) List.t
      }
  end =
    Copattern

  (** External computation-level contexts. *)
  and Context : sig
    (** [{ Context.bindings; _ }] is the computation-level context

        - [^] if [bindings = \[\]]
        - [x1 : a1, x2 : a2, ..., xn : an] if
          [bindings = \[("x1", a1); ("x2", a2); ...; ("xn", an)\]] *)
    type t =
      { location : Location.t
      ; bindings : (Identifier.t * Typ.t) List.t
      }
  end =
    Context
end

(** {1 External Harpoon Syntax} *)

module Harpoon = struct
  module rec Proof : sig
    type t =
      | Incomplete of
          { location : Location.t
          ; label : Identifier.t Option.t
          }
      | Command of
          { location : Location.t
          ; command : Command.t
          ; body : Proof.t
          }
      | Directive of
          { location : Location.t
          ; directive : Directive.t
          }
  end =
    Proof

  and Command : sig
    type t =
      | By of
          { location : Location.t
          ; expression : Comp.Expression.t
          ; assignee : Identifier.t
          }
      | Unbox of
          { location : Location.t
          ; expression : Comp.Expression.t
          ; assignee : Identifier.t
          ; modifier : [ `Strengthened ] Option.t
          }
  end =
    Command

  and Directive : sig
    type t =
      | Intros of
          { location : Location.t
          ; hypothetical : Hypothetical.t
          }
      | Solve of
          { location : Location.t
          ; solution : Comp.Expression.t
          }
      | Split of
          { location : Location.t
          ; scrutinee : Comp.Expression.t
          ; branches : Split_branch.t List1.t
          }
      | Impossible of
          { location : Location.t
          ; scrutinee : Comp.Expression.t
          }
      | Suffices of
          { location : Location.t
          ; scrutinee : Comp.Expression.t
          ; branches : Suffices_branch.t List.t
          }
  end =
    Directive

  and Split_branch : sig
    type t =
      { location : Location.t
      ; label : Split_branch.Label.t
      ; body : Hypothetical.t
      }

    module Label : sig
      type t =
        | Lf_constant of
            { location : Location.t
            ; identifier : Qualified_identifier.t
            }
        | Comp_constant of
            { location : Location.t
            ; identifier : Qualified_identifier.t
            }
        | Bound_variable of { location : Location.t }
        | Empty_context of { location : Location.t }
        | Extended_context of
            { location : Location.t
            ; schema_element : Int.t  (** 1-based *)
            }
        | Parameter_variable of
            { location : Location.t
            ; schema_element : Int.t  (** 1-based *)
            ; projection : Int.t Option.t  (** 1-based *)
            }
    end
  end =
    Split_branch

  and Suffices_branch : sig
    type t =
      { location : Location.t
      ; goal : Comp.Typ.t
      ; proof : Proof.t
      }
  end =
    Suffices_branch

  and Hypothetical : sig
    type t =
      { location : Location.t
      ; meta_context : Meta.Context.t
      ; comp_context : Comp.Context.t
      ; proof : Proof.t
      }
  end =
    Hypothetical

  module Repl = struct
    module Command = struct
      type t =
        (* Administrative commands *)
        | Rename of
            { location : Location.t
            ; rename_from : Identifier.t
            ; rename_to : Identifier.t
            ; level : [ `meta | `comp ]
            }
        | Toggle_automation of
            { location : Location.t
            ; kind : [ `auto_intros | `auto_solve_trivial ]
            ; change : [ `on | `off | `toggle ]
            }
        | Type of
            { location : Location.t
            ; scrutinee : Comp.Expression.t
            }
        | Info of
            { location : Location.t
            ; kind : [ `prog ]
            ; object_identifier : Qualified_identifier.t
            }
        | Select_theorem of
            { location : Location.t
            ; theorem : Qualified_identifier.t
            }
        | Theorem of
            { location : Location.t
            ; subcommand :
                [ `list
                | `defer
                | `show_ihs
                | `show_proof
                | `dump_proof of String.t  (** File path *)
                ]
            }
        | Session of
            { location : Location.t
            ; subcommand : [ `list | `defer | `create | `serialize ]
            }
        | Subgoal of
            { location : Location.t
            ; subcommand : [ `list | `defer ]
            }
        | Undo of { location : Location.t }
        | Redo of { location : Location.t }
        | History of { location : Location.t }
        | Translate of
            { location : Location.t
            ; theorem : Qualified_identifier.t
            }
        (* Actual tactics *)
        | Intros of
            { location : Location.t
            ; introduced_variables : Identifier.t List1.t Option.t
            }
        | Split of
            { location : Location.t
            ; scrutinee : Comp.Expression.t
            }
        | Invert of
            { location : Location.t
            ; scrutinee : Comp.Expression.t
            }
        | Impossible of
            { location : Location.t
            ; scrutinee : Comp.Expression.t
            }
        | Msplit of
            { location : Location.t
            ; identifier : Identifier.t
            }
        | Solve of
            { location : Location.t
            ; solution : Comp.Expression.t
            }
        | Unbox of
            { location : Location.t
            ; expression : Comp.Expression.t
            ; assignee : Identifier.t
            ; modifier : [ `Strengthened ] Option.t
            }
        | By of
            { location : Location.t
            ; expression : Comp.Expression.t
            ; assignee : Identifier.t
            }
        | Suffices of
            { location : Location.t
            ; implication : Comp.Expression.t
            ; goal_premises :
                [ `exact of Comp.Typ.t | `infer of Location.t ] List.t
            }
        | Help of { location : Location.t }
    end
  end
end

(** {1 External Signature Syntax} *)

module Signature = struct
  (** Signature pragmas for setting compilation parameters

      Plain pragmas may be interspersed between signature declarations. *)
  module Pragma = struct
    type t =
      | Name of
          { location : Location.t
          ; constant : Qualified_identifier.t
          ; meta_variable_base : Identifier.t
          ; computation_variable_base : Identifier.t Option.t
          }
          (** [Name { constant = c; meta_variable_base = u; computation_variable_base = Option.Some x; _ }]
              is the pragma [--name c u x.] for configuring the
              name-generation settings for meta-variables and
              computation-level variables generated for objects of type [c]. *)
      | Default_associativity of
          { location : Location.t
          ; associativity : Associativity.t
          }
          (** [Default_associativity { associativity; _ }] is the pragma
              [--assoc <associativity>.] where [<associativity>] is either
              [left], [right] or [none]. This pragma assigns the default
              associativity for infix constants declared afterwards. *)
      | Prefix_fixity of
          { location : Location.t
          ; constant : Qualified_identifier.t
          ; precedence : Int.t Option.t
          }
          (** [Prefix_fixity { constant = c; precedence; _ }] is the pragma
              [--prefix c precedence.] for configuring the constant [c] to be
              parsed as a prefix operator with [precedence].

              If a precedence is already assigned to [c], and
              [precedence = Option.None], then the pre-existing precedence is
              used. *)
      | Infix_fixity of
          { location : Location.t
          ; constant : Qualified_identifier.t
          ; precedence : Int.t Option.t
          ; associativity : Associativity.t Option.t
          }
          (** [Infix_fixity { constant = c; precedence; associativity; _ }]
              is the pragma [--infix c precedence associativity.] for
              configuring the constant [c] to be parsed as an infix operator
              with [precedence] and [associativity].

              - If a precedence is already assigned to [c], and
                [precedence = Option.None], then the pre-existing precedence
                is used.
              - If [associativity = Option.None], then the associativity
                defaults to the associativity configured by the
                [--default_associativity assoc.] pragma, or
                [Associativity.Non_associative] if that pragma was never
                used. *)
      | Postfix_fixity of
          { location : Location.t
          ; constant : Qualified_identifier.t
          ; precedence : Int.t Option.t
          }
          (** [Postfix_fixity { constant = c; precedence; _ }] is the pragma
              [--postfix c precedence.] for configuring the constant [c] to
              be parsed as a postfix operator with [precedence].

              If a precedence is already assigned to [c], and
              [precedence = Option.None], then the pre-existing precedence is
              used. *)
      | Not of { location : Location.t }
          (** [Not _] is the pragma [--not] which asserts that the
              declaration that follows it fails reconstruction. *)
      | Open_module of
          { location : Location.t
          ; module_identifier : Qualified_identifier.t
          }
          (** [Open { module_identifier; _ }] is the pragma
              [--open module_identifier.] for opening the module
              [module_identifier], which adds its values to the current
              scope. *)
      | Abbreviation of
          { location : Location.t
          ; module_identifier : Qualified_identifier.t
          ; abbreviation : Identifier.t
          }
          (** [Abbreviation { module_identifier; abbreviation; _ }] is the
              pragma [--abbrev module_identifier abbreviation.] for defining
              the alias [abbreviation] for the module [module_identifier]. *)
  end

  (** Global signature pragmas for setting compilation parameters

      Global pragmas must appear at the beginning of a signature. They act
      like command-line interface flags. *)
  module Global_pragma = struct
    type t =
      | No_strengthening of { location : Location.t }
          (** [No_strengthening _] is the pragma [--nostrengthen] for
              globally disabling strengthening during LF reconstruction. *)
      | Warn_on_coverage_error of { location : Location.t }
          (** [Warn_on_coverage_error _] is the pragma [--warncoverage] for
              enabling coverage checking and raising warnings on coverage
              errors. *)
      | Raise_error_on_coverage_error of { location : Location.t }
          (** [Raise_error_on_coverage_error _] is the pragma [--coverage]
              for enabling coverage checking and raising errors on coverage
              errors. *)
  end

  (** Totality declarations and orderings for configuring the totality
      checker for theorems and proofs.

      For instance, the named totality declaration

      {[
        / total [x y z] (f x y z w) /
      ]}

      specifies that the function named [f] is checked for totality using the
      lexical ordering [\[x y z\]] of its arguments. *)
  module rec Totality : sig
    module rec Declaration : sig
      type t =
        | Trust of { location : Location.t }
            (** [Trust _] is the totality declaration [trust] which indicates
                that the totality of the annotated function is not checked. *)
        | Numeric of
            { location : Location.t
            ; order : Int.t Order.t Option.t
            }
            (** - [Numeric { order = Option.None; _ }] is the totality
                  declaration [total] which indicates that the annotated
                  function should be checked for totality without totality
                  argument.
                - [Numeric { order = Option.Some order; _ }] is the totality
                  declaration [total order] which indicates that the
                  annotated function should be checked for totality with
                  [order]. *)
        | Named of
            { location : Location.t
            ; order : Identifier.t Order.t Option.t
            ; program : Identifier.t
            ; argument_labels : Identifier.t Option.t List.t
            }
            (** - [Named { order = Option.None; program = "f"; argument_labels = \["x1"; "x2"; ...; "xn"\]; _ }]
                  is the totality declaration [total (f x1 x2 ... xn)] which
                  indicates that the annotated function should be checked for
                  totality without totality argument.
                - [Named { order = Option.Some order; program = "f"; argument_labels = \["x1"; "x2"; ...; "xn"\]; _ }]
                  is the totality declaration [total x (f x1 x2 ... xn)]
                  which indicates that the annotated function should be
                  checked for totality with [order]. *)
    end

    (** Totality argument orderings for totality-checking. *)
    and Order : sig
      type 'a t =
        | Argument of
            { location : Location.t
            ; argument : 'a
            }
        | Lexical_ordering of
            { location : Location.t
            ; arguments : 'a Order.t List1.t
            }
        | Simultaneous_ordering of
            { location : Location.t
            ; arguments : 'a Order.t List1.t
            }
    end
  end =
    Totality

  module rec Declaration : sig
    (** Parsed signature element *)
    type t =
      | Typ of
          { location : Location.t
          ; identifier : Identifier.t
          ; kind : LF.Kind.t
          }  (** LF type-level constant declaration *)
      | Const of
          { location : Location.t
          ; identifier : Identifier.t
          ; typ : LF.Typ.t
          }  (** LF term-level constant declaration *)
      | CompTyp of
          { location : Location.t
          ; identifier : Identifier.t
          ; kind : Comp.Kind.t
          ; datatype_flavour : [ `Inductive | `Stratified ]
          }  (** Computation-level data type constant declaration *)
      | CompCotyp of
          { location : Location.t
          ; identifier : Identifier.t
          ; kind : Comp.Kind.t
          }  (** Computation-level codata type constant declaration *)
      | CompConst of
          { location : Location.t
          ; identifier : Identifier.t
          ; typ : Comp.Typ.t
          }  (** Computation-level type constructor declaration *)
      | CompDest of
          { location : Location.t
          ; identifier : Identifier.t
          ; observation_type : Comp.Typ.t
          ; return_type : Comp.Typ.t
          }  (** Computation-level type destructor declaration *)
      | Schema of
          { location : Location.t
          ; identifier : Identifier.t
          ; schema : Meta.Schema.t
          }  (** Declaration of a specification for a set of contexts *)
      | Recursive_declarations of
          { location : Location.t
          ; declarations : Declaration.t List1.t
          }  (** Recursive declaration(s) *)
      | Theorem of
          { location : Location.t
          ; identifier : Identifier.t
          ; typ : Comp.Typ.t
          ; order : Totality.Declaration.t Option.t
          ; body : Comp.Expression.t
          }  (** Beluga theorem declaration. *)
      | Proof of
          { location : Location.t
          ; identifier : Identifier.t
          ; typ : Comp.Typ.t
          ; order : Totality.Declaration.t Option.t
          ; body : Harpoon.Proof.t
          }  (** Harpoon proof declaration. *)
      | CompTypAbbrev of
          { location : Location.t
          ; identifier : Identifier.t
          ; kind : Comp.Kind.t
          ; typ : Comp.Typ.t
          }  (** Declaration for a computation-level type function. *)
      | Val of
          { location : Location.t
          ; identifier : Identifier.t
          ; typ : Comp.Typ.t Option.t
          ; expression : Comp.Expression.t
          }  (** Computation-level value declaration *)
      | Query of
          { location : Location.t
          ; identifier : Identifier.t Option.t
          ; meta_context : Meta.Context.t
          ; typ : LF.Typ.t
          ; expected_solutions : Int.t Option.t
          ; maximum_tries : Int.t Option.t
          }  (** Logic programming query on an LF type *)
      | Module of
          { location : Location.t
          ; identifier : Identifier.t
          ; entries : Entry.t List.t
          }  (** Namespace declaration for other declarations *)
  end =
    Declaration

  and Entry : sig
    type t =
      | Pragma of
          { location : Location.t
          ; pragma : Pragma.t
          }
      | Declaration of
          { location : Location.t
          ; declaration : Declaration.t
          }
      | Comment of
          { location : Location.t
          ; content : String.t
          }  (** Documentation comment *)
  end =
    Entry

  type t =
    { global_pragmas : Global_pragma.t List.t
    ; entries : Entry.t List.t
    }
end

(** {1 Type Aliases} *)

(** {2 LF} *)

(** @canonical Synext.lf_kind *)
type lf_kind = LF.Kind.t

(** @canonical Synext.lf_typ *)
type lf_typ = LF.Typ.t

(** @canonical Synext.lf_term *)
type lf_term = LF.Term.t

(** {2 Contextual LF} *)

(** @canonical Synext.clf_typ *)
type clf_typ = CLF.Typ.t

(** @canonical Synext.clf_term *)
type clf_term = CLF.Term.t

(** @canonical Synext.clf_term_pattern *)
type clf_term_pattern = CLF.Term.Pattern.t

(** @canonical Synext.clf_substitution *)
type clf_substitution = CLF.Substitution.t

(** @canonical Synext.clf_substitution_pattern *)
type clf_substitution_pattern = CLF.Substitution.Pattern.t

(** @canonical Synext.clf_context *)
type clf_context = CLF.Context.t

(** @canonical Synext.clf_context_pattern *)
type clf_context_pattern = CLF.Context.Pattern.t

(** {2 Meta-level} *)

(** @canonical Synext.meta_typ *)
type meta_typ = Meta.Typ.t

(** @canonical Synext.meta_object *)
type meta_object = Meta.Object.t

(** @canonical Synext.meta_pattern *)
type meta_pattern = Meta.Pattern.t

(** @canonical Synext.schema *)
type schema = Meta.Schema.t

(** @canonical Synext.meta_context *)
type meta_context = Meta.Context.t

(** {2 Computation-level} *)

(** @canonical Synext.comp_kind *)
type comp_kind = Comp.Kind.t

(** @canonical Synext.comp_typ *)
type comp_typ = Comp.Typ.t

(** @canonical Synext.comp_expression *)
type comp_expression = Comp.Expression.t

(** @canonical Synext.comp_pattern *)
type comp_pattern = Comp.Pattern.t

(** @canonical Synext.comp_copattern *)
type comp_copattern = Comp.Copattern.t

(** @canonical Synext.comp_context *)
type comp_context = Comp.Context.t

(** {2 Harpoon} *)

(** @canonical Synext.harpoon_command *)
type harpoon_command = Harpoon.Command.t

(** @canonical Synext.harpoon_directive *)
type harpoon_directive = Harpoon.Directive.t

(** @canonical Synext.harpoon_proof *)
type harpoon_proof = Harpoon.Proof.t

(** @canonical Synext.harpoon_hypothetical *)
type harpoon_hypothetical = Harpoon.Hypothetical.t

(** @canonical Synext.harpoon_repl_command *)
type harpoon_repl_command = Harpoon.Repl.Command.t

(** @canonical Synext.harpoon_split_branch *)
type harpoon_split_branch = Harpoon.Split_branch.t

(** @canonical Synext.harpoon_split_branch_label *)
type harpoon_split_branch_label = Harpoon.Split_branch.Label.t

(** @canonical Synext.harpoon_suffices_branch *)
type harpoon_suffices_branch = Harpoon.Suffices_branch.t

(** {2 Signature} *)

(** @canonical Synext.signature_pragma *)
type signature_pragma = Signature.Pragma.t

(** @canonical Synext.signature_global_pragma *)
type signature_global_pragma = Signature.Global_pragma.t

(** @canonical Synext.signature_totality_declaration *)
type signature_totality_declaration = Signature.Totality.Declaration.t

(** @canonical Synext.'argument *)
type 'argument signature_totality_order =
  'argument Signature.Totality.Order.t

(** @canonical Synext.signature_declaration *)
type signature_declaration = Signature.Declaration.t

(** @canonical Synext.signature_entry *)
type signature_entry = Signature.Entry.t

(** @canonical Synext.signature *)
type signature = Signature.t
