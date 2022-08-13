(* Parser Syntax *)

open Support

(** {1 Parser LF Syntax}

    The intermediate representation of LF kinds, types and terms to delay the
    handling of data-dependent aspects of the grammar.

    OCaml constructor names prefixed with `Raw' require data-dependent
    disambiguation or reduction during the elaboration to the external
    syntax.

    The parsers associated with these types are only intended to be used in
    the definition of LF type-level or term-level constants. This is a weak,
    representational function space without case analysis or recursion. *)
module LF = struct
  (** LF kinds, types and terms blurred together. *)
  module rec Object : sig
    type t =
      | RawIdentifier of
          { location : Location.t
          ; identifier : Identifier.t
          }
      | RawQualifiedIdentifier of
          { location : Location.t
          ; identifier : QualifiedIdentifier.t
          }
      | RawType of { location : Location.t }
      | RawHole of { location : Location.t }
      | RawPi of
          { location : Location.t
          ; parameter_identifier : Identifier.t Option.t
          ; parameter_sort : Object.t Option.t
          ; body : Object.t
          }
      | RawLambda of
          { location : Location.t
          ; parameter_identifier : Identifier.t Option.t
          ; parameter_sort : Object.t Option.t
          ; body : Object.t
          }
      | RawForwardArrow of
          { location : Location.t
          ; domain : Object.t
          ; range : Object.t
          }
      | RawBackwardArrow of
          { location : Location.t
          ; domain : Object.t
          ; range : Object.t
          }
      | RawAnnotated of
          { location : Location.t
          ; object_ : Object.t
          ; sort : Object.t
          }
      | RawApplication of
          { location : Location.t
          ; objects : Object.t List2.t
          }
      | RawParenthesized of
          { location : Location.t
          ; object_ : Object.t
          }
  end =
    Object

  let location_of_object object_ =
    match object_ with
    | Object.RawIdentifier { location; _ }
    | Object.RawQualifiedIdentifier { location; _ }
    | Object.RawType { location; _ }
    | Object.RawHole { location; _ }
    | Object.RawPi { location; _ }
    | Object.RawLambda { location; _ }
    | Object.RawForwardArrow { location; _ }
    | Object.RawBackwardArrow { location; _ }
    | Object.RawAnnotated { location; _ }
    | Object.RawApplication { location; _ }
    | Object.RawParenthesized { location; _ } -> location

  let rec pp_object ppf object_ =
    match object_ with
    | Object.RawIdentifier { identifier; _ } ->
      Format.fprintf ppf "%a" Identifier.pp identifier
    | Object.RawQualifiedIdentifier { identifier; _ } ->
      Format.fprintf ppf "%a" QualifiedIdentifier.pp identifier
    | Object.RawType _ -> Format.fprintf ppf "type"
    | Object.RawHole _ -> Format.fprintf ppf "_"
    | Object.RawPi
        { parameter_identifier = Option.None
        ; parameter_sort = Option.None
        ; body
        ; _
        } -> Format.fprintf ppf "@[<2>{@ _@ }@ %a@]" pp_object body
    | Object.RawPi
        { parameter_identifier = Option.None
        ; parameter_sort = Option.Some parameter_sort
        ; body
        ; _
        } ->
      Format.fprintf ppf "@[<2>{@ _@ :@ %a@ }@ %a@]" pp_object parameter_sort
        pp_object body
    | Object.RawPi
        { parameter_identifier = Option.Some parameter_identifier
        ; parameter_sort = Option.None
        ; body
        ; _
        } ->
      Format.fprintf ppf "@[<2>{@ %a@ }@ %a@]" Identifier.pp
        parameter_identifier pp_object body
    | Object.RawPi
        { parameter_identifier = Option.Some parameter_identifier
        ; parameter_sort = Option.Some parameter_sort
        ; body
        ; _
        } ->
      Format.fprintf ppf "@[<2>{@ %a@ :@ %a@ }@ %a@]" Identifier.pp
        parameter_identifier pp_object parameter_sort pp_object body
    | Object.RawLambda
        { parameter_identifier = Option.None
        ; parameter_sort = Option.None
        ; body
        ; _
        } -> Format.fprintf ppf "@[<2>\\_.@ %a@]" pp_object body
    | Object.RawLambda
        { parameter_identifier = Option.None
        ; parameter_sort = Option.Some parameter_sort
        ; body
        ; _
        } ->
      Format.fprintf ppf "@[<2>\\_:%a.@ %a@]" pp_object parameter_sort
        pp_object body
    | Object.RawLambda
        { parameter_identifier = Option.Some parameter_identifier
        ; parameter_sort = Option.None
        ; body
        ; _
        } ->
      Format.fprintf ppf "@[<2>\\%a.@ %a@]" Identifier.pp
        parameter_identifier pp_object body
    | Object.RawLambda
        { parameter_identifier = Option.Some parameter_identifier
        ; parameter_sort = Option.Some parameter_sort
        ; body
        ; _
        } ->
      Format.fprintf ppf "@[<2>\\%a:%a.@ %a@]" Identifier.pp
        parameter_identifier pp_object parameter_sort pp_object body
    | Object.RawForwardArrow { domain; range; _ } ->
      Format.fprintf ppf "@[<2>%a@ ->@ %a@]" pp_object domain pp_object range
    | Object.RawBackwardArrow { domain; range; _ } ->
      Format.fprintf ppf "@[<2>%a@ <-@ %a@]" pp_object domain pp_object range
    | Object.RawAnnotated { object_; sort; _ } ->
      Format.fprintf ppf "@[<2>%a@ :@ %a@]" pp_object object_ pp_object sort
    | Object.RawApplication { objects; _ } ->
      Format.fprintf ppf "@[<2>%a@]"
        (List2.pp ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ") pp_object)
        objects
    | Object.RawParenthesized { object_; _ } ->
      Format.fprintf ppf "@[<2>(%a)@]" pp_object object_

  let rec pp_object_debug ppf object_ =
    match object_ with
    | Object.RawIdentifier { identifier; _ } ->
      Format.fprintf ppf "%a" Identifier.pp identifier
    | Object.RawQualifiedIdentifier { identifier; _ } ->
      Format.fprintf ppf "%a" QualifiedIdentifier.pp identifier
    | Object.RawType _ -> Format.fprintf ppf "type"
    | Object.RawHole _ -> Format.fprintf ppf "_"
    | Object.RawPi
        { parameter_identifier = Option.None
        ; parameter_sort = Option.None
        ; body
        ; _
        } -> Format.fprintf ppf "@[<2>Pi({@ _@ }@ %a)@]" pp_object_debug body
    | Object.RawPi
        { parameter_identifier = Option.None
        ; parameter_sort = Option.Some parameter_sort
        ; body
        ; _
        } ->
      Format.fprintf ppf "@[<2>Pi({@ _@ :@ %a@ }@ %a)@]" pp_object_debug
        parameter_sort pp_object_debug body
    | Object.RawPi
        { parameter_identifier = Option.Some parameter_identifier
        ; parameter_sort = Option.None
        ; body
        ; _
        } ->
      Format.fprintf ppf "@[<2>Pi({@ %a@ }@ %a)@]" Identifier.pp
        parameter_identifier pp_object_debug body
    | Object.RawPi
        { parameter_identifier = Option.Some parameter_identifier
        ; parameter_sort = Option.Some parameter_sort
        ; body
        ; _
        } ->
      Format.fprintf ppf "@[<2>Pi({@ %a@ :@ %a@ }@ %a)@]" Identifier.pp
        parameter_identifier pp_object_debug parameter_sort pp_object_debug
        body
    | Object.RawLambda
        { parameter_identifier = Option.None
        ; parameter_sort = Option.None
        ; body
        ; _
        } ->
      Format.fprintf ppf "@[<2>Lambda(\\_.@ %a)@]" pp_object_debug body
    | Object.RawLambda
        { parameter_identifier = Option.None
        ; parameter_sort = Option.Some parameter_sort
        ; body
        ; _
        } ->
      Format.fprintf ppf "@[<2>Lambda(\\_:%a.@ %a)@]" pp_object_debug
        parameter_sort pp_object_debug body
    | Object.RawLambda
        { parameter_identifier = Option.Some parameter_identifier
        ; parameter_sort = Option.None
        ; body
        ; _
        } ->
      Format.fprintf ppf "@[<2>Lambda(\\%a.@ %a)@]" Identifier.pp
        parameter_identifier pp_object_debug body
    | Object.RawLambda
        { parameter_identifier = Option.Some parameter_identifier
        ; parameter_sort = Option.Some parameter_sort
        ; body
        ; _
        } ->
      Format.fprintf ppf "@[<2>Lambda(\\%a:%a.@ %a)@]" Identifier.pp
        parameter_identifier pp_object_debug parameter_sort pp_object_debug
        body
    | Object.RawForwardArrow { domain; range; _ } ->
      Format.fprintf ppf "@[<2>ForwardArrow(%a@ ->@ %a)@]" pp_object_debug
        domain pp_object_debug range
    | Object.RawBackwardArrow { domain; range; _ } ->
      Format.fprintf ppf "@[<2>BackwardArrow(%a@ <-@ %a)@]" pp_object_debug
        domain pp_object_debug range
    | Object.RawAnnotated { object_; sort; _ } ->
      Format.fprintf ppf "@[<2>Annotated(%a@ :@ %a)@]" pp_object_debug
        object_ pp_object_debug sort
    | Object.RawApplication { objects; _ } ->
      Format.fprintf ppf "@[<2>Application(%a)@]"
        (List2.pp
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
           pp_object_debug)
        objects
    | Object.RawParenthesized { object_; _ } ->
      Format.fprintf ppf "@[<2>Parenthesized(%a)@]" pp_object_debug object_
end

(** {1 Parser Contextual LF Syntax} *)
module CLF = struct
  include Syncom.LF

  type kind =
    | Typ of { location : Location.t }
    | ArrKind of
        { location : Location.t
        ; domain : typ
        ; range : kind
        }
    | PiKind of
        { location : Location.t
        ; parameter_name : Name.t
        ; parameter_type : typ
        ; range : kind
        }

  and typ_decl =
    | TypDecl of Name.t * typ
    | TypDeclOpt of Name.t

  and cltyp =
    | MTyp of typ
    | PTyp of typ
    | STyp of svar_class * dctx

  and ctyp =
    | ClTyp of Location.t * cltyp * dctx
    | CTyp of Location.t * Name.t

  and ctyp_decl =
    | Decl of Name.t * ctyp * Plicity.t
    | DeclOpt of Name.t

  and typ =
    | Atom of
        { location : Location.t
        ; head : Name.t
        ; spine : (Location.t * term) list
        }
    | ArrTyp of
        { location : Location.t
        ; domain : typ
        ; range : typ
        }
    | PiTyp of
        { location : Location.t
        ; parameter_name : Name.t
        ; parameter_type : typ
        ; range : typ
        }
    | Sigma of
        { location : Location.t
        ; block : typ_rec
        }
    | AtomTerm of
        { location : Location.t
        ; term : term
        }

  and term =
    | Lam of
        { location : Location.t
        ; parameter_name : Name.t
        ; body : term
        }
    | Root of
        { location : Location.t
        ; head : head
        ; spine : (Location.t * term) list
        }
    | Tuple of
        { location : Location.t
        ; tuple : term List1.t
        }
    | LFHole of
        { location : Location.t
        ; label : string option
        }
    | Ann of
        { location : Location.t
        ; term : term
        ; typ : typ
        }
    | TList of
        { location : Location.t
        ; terms : term list
        }
    | NTyp of
        { location : Location.t
        ; typ : typ
        }

  and head =
    | Name of Location.t * Name.t * sub option
    | Hole of Location.t
    | PVar of Location.t * Name.t * sub option
    | Proj of Location.t * head * proj

  and proj =
    | ByPos of int
    | ByName of Name.t

  and sub_start =
    | EmptySub of Location.t
    | Id of Location.t
    | SVar of Location.t * Name.t * sub option

  and sub = sub_start * term list

  and typ_rec =
    | SigmaLast of Name.t option * typ
    | SigmaElem of Name.t * typ * typ_rec

  and dctx =
    | Null
    | CtxVar of Location.t * Name.t
    | DDec of dctx * typ_decl
    | CtxHole

  and sch_elem = SchElem of Location.t * typ_decl ctx * typ_rec

  and schema = Schema of sch_elem list

  and mctx = ctyp_decl ctx

  type mfront =
    | ClObj of dctx * sub
    (* ClObj doesn't *really* contain just a substitution. The problem is
       that syntactically, we can't tell whether `[psi |- a]' is a boxed
       object or substitution! So it turns out that, syntactically,
       substitutions encompass both possibilities: a substitution beginning
       with EmptySub and having just one term term in it can represent a
       boxed term. We disambiguate substitutions from terms at a later
       time. *)
    | CObj of dctx

  let loc_of_head = function
    | Name (l, _, _) -> l
    | Hole l -> l
    | PVar (l, _, _) -> l
    | Proj (l, _, _) -> l
end

(** {1 Parser Computation Syntax} *)
module Comp = struct
  include Syncom.Comp

  type kind =
    | Ctype of { location : Location.t }
    | ArrKind of
        { location : Location.t
        ; domain : CLF.ctyp
        ; range : kind
        }
    | PiKind of
        { location : Location.t
        ; parameter_name : Name.t
        ; parameter_type : CLF.ctyp
        ; plicity : Plicity.t
        ; range : kind
        }

  type meta_obj = Location.t * CLF.mfront

  type meta_typ = CLF.ctyp

  type typ =
    | TypBase of
        { location : Location.t
        ; head : Name.t
        ; spine : meta_obj list
        }
    | TypBox of
        { location : Location.t
        ; typ : meta_typ
        }
    | TypArr of
        { location : Location.t
        ; domain : typ
        ; range : typ
        }
    | TypCross of
        { location : Location.t
        ; typs : typ List2.t
        }
    | TypPiBox of
        { location : Location.t
        ; parameter_name : Name.t
        ; parameter_type : CLF.ctyp
        ; plicity : Plicity.t
        ; range : typ
        }

  and exp =
    | Fn of
        { location : Location.t
        ; parameter_name : Name.t
        ; body : exp
        }
    | Fun of
        { location : Location.t
        ; branches : branch List1.t
        }
    | MLam of
        { location : Location.t
        ; parameter_name : Name.t
        ; body : exp
        }
    | Tuple of
        { location : Location.t
        ; expressions : exp List2.t
        }
    | Let of
        { location : Location.t
        ; pattern : pattern
        ; assignee : exp
        ; body : exp
        }
    | Box of
        { location : Location.t
        ; obj : meta_obj
        }
    | Impossible of
        { location : Location.t
        ; expression : exp
        }
    | Case of
        { location : Location.t
        ; check_exhaustiveness : bool
        ; scrutinee : exp
        ; branches : branch List1.t
        }
    | Hole of
        { location : Location.t
        ; label : string option
        }
    | BoxHole of { location : Location.t }
    | Name of
        { location : Location.t
        ; name : Name.t
        }
    | Apply of
        { location : Location.t
        ; applicand : exp
        ; argument : exp
        }

  and pattern =
    | PatMetaObj of
        { location : Location.t
        ; obj : meta_obj
        }
    | RawPatApplication of
        { location : Location.t
        ; patterns : pattern List1.t
        }
    | PatName of
        { location : Location.t
        ; name : Name.t
        }
    | PatTuple of
        { location : Location.t
        ; patterns : pattern List2.t
        }
    | PatAnn of
        { location : Location.t
        ; pattern : pattern
        ; typ : typ
        }
    | PatMAnn of
        { location : Location.t
        ; pattern : pattern
        ; typs : (Name.t * CLF.ctyp) List1.t
        }
    | PatObs of
        { location : Location.t
        ; name : Name.t
        }

  and branch =
    | Branch of
        { location : Location.t
        ; pattern : pattern
        ; body : exp
        }

  type suffices_typ = typ generic_suffices_typ

  type named_order = Name.t generic_order

  type numeric_order = int generic_order

  type total_dec =
    | NumericTotal of Location.t * numeric_order option
    | NamedTotal of
        Location.t * named_order option * Name.t * Name.t option list
    | Trust of Location.t

  type ctyp_decl = CTypDecl of Name.t * typ

  type gctx = ctyp_decl CLF.ctx

  type hypotheses =
    { cD : CLF.mctx
    ; cG : gctx
    }

  type proof =
    | Incomplete of Location.t * string option
    | Command of Location.t * command * proof
    | Directive of Location.t * directive

  and command =
    | By of Location.t * exp * Name.t
    | Unbox of Location.t * exp * Name.t * unbox_modifier option

  and directive =
    | Intros of Location.t * hypothetical
    | Solve of Location.t * exp
    | Split of Location.t * exp * split_branch list
    | Suffices of Location.t * exp * (Location.t * typ * proof) list

  and split_branch =
    { case_label : case_label
    ; branch_body : hypothetical
    ; split_branch_loc : Location.t
    }

  and hypothetical =
    { hypotheses : hypotheses
    ; proof : proof
    ; hypothetical_loc : Location.t
    }

  type thm =
    | Program of exp
    | Proof of proof

  let loc_of_exp = function
    | Fn { location; _ }
    | Fun { location; _ }
    | MLam { location; _ }
    | Tuple { location; _ }
    | Let { location; _ }
    | Box { location; _ }
    | Impossible { location; _ }
    | Case { location; _ }
    | Hole { location; _ }
    | BoxHole { location; _ }
    | Name { location; _ }
    | Apply { location; _ } -> location
end

(** {1 Harpoon Command Syntax} *)
module Harpoon = struct
  type defer_kind =
    [ `subgoal
    | `theorem
    ]

  type invoke_kind =
    [ `ih
    | `lemma
    ]

  type split_kind =
    [ `split
    | `invert
    | `impossible
    ]

  type level =
    [ `meta
    | `comp
    ]

  type automation_kind =
    [ `auto_intros
    | `auto_solve_trivial
    ]

  type automation_change =
    [ `on
    | `off
    | `toggle
    ]

  type basic_command =
    [ `list
    | `defer
    ]

  type info_kind = [ `prog ]

  type command =
    (* Administrative commands *)
    | Rename of
        { rename_from : Name.t
        ; rename_to : Name.t
        ; level : level
        }
    | ToggleAutomation of automation_kind * automation_change
    | Type of Comp.exp
    | Info of info_kind * Name.t
    | SelectTheorem of Name.t
    | Theorem of
        [ basic_command | `show_ihs | `show_proof | `dump_proof of string ]
    | Session of [ basic_command | `create | `serialize ]
    | Subgoal of basic_command
    | Undo
    | Redo
    | History
    | Translate of Name.t
    (* Actual tactics *)
    | Intros of
        string list option (* list of names for introduced variables *)
    | Split of split_kind * Comp.exp (* the expression to split on *)
    | MSplit of Location.t * Name.t (* split on a metavariable *)
    | Solve of
        Comp.exp (* the expression to solve the current subgoal with *)
    | Unbox of Comp.exp * Name.t * Comp.unbox_modifier option
    | By of Comp.exp * Name.t
    | Suffices of Comp.exp * Comp.suffices_typ list
    | Help
end

(** {1 Parser Signature Syntax} *)
module Sgn = struct
  type datatype_flavour =
    | InductiveDatatype
    | StratifiedDatatype

  type precedence = int

  type pragma =
    | NamePrag of
        { constant : Name.t
        ; meta_name : string
        ; comp_name : string option
        }
    | FixPrag of
        { constant : Name.t
        ; fixity : Fixity.t
        ; precedence : precedence
        ; associativity : Associativity.t option
        }
    | NotPrag
    | DefaultAssocPrag of { associativity : Associativity.t }
    | OpenPrag of string list
    | AbbrevPrag of string list * string

  (* Pragmas that need to be declared first *)
  type global_pragma =
    | NoStrengthen
    | Coverage of [ `Error | `Warn ]

  type thm_decl =
    | Theorem of
        { location : Location.t
        ; name : Name.t
        ; typ : Comp.typ
        ; order : Comp.total_dec option
        ; body : Comp.thm
        }

  (** Parsed signature element *)
  type decl =
    | Typ of
        { location : Location.t
        ; identifier : Name.t
        ; kind : LF.Object.t
        }  (** LF type family declaration *)
    | Const of
        { location : Location.t
        ; identifier : Name.t
        ; typ : LF.Object.t
        }  (** LF type constant decalaration *)
    | CompTyp of
        { location : Location.t
        ; identifier : Name.t
        ; kind : Comp.kind
        ; datatype_flavour : datatype_flavour
        }  (** Computation-level data type constant declaration *)
    | CompCotyp of
        { location : Location.t
        ; identifier : Name.t
        ; kind : Comp.kind
        }  (** Computation-level codata type constant declaration *)
    | CompConst of
        { location : Location.t
        ; identifier : Name.t
        ; typ : Comp.typ
        }  (** Computation-level type constructor declaration *)
    | CompDest of
        { location : Location.t
        ; identifier : Name.t
        ; mctx : CLF.mctx
        ; observation_typ : Comp.typ
        ; return_typ : Comp.typ
        }  (** Computation-level type destructor declaration *)
    | CompTypAbbrev of
        { location : Location.t
        ; identifier : Name.t
        ; kind : Comp.kind
        ; typ : Comp.typ
        }  (** Synonym declaration for computation-level type *)
    | Schema of
        { location : Location.t
        ; identifier : Name.t
        ; schema : CLF.schema
        }  (** Declaration of a specification for a set of contexts *)
    | Pragma of
        { location : Location.t
        ; pragma : pragma
        }  (** Compiler directive *)
    | GlobalPragma of
        { location : Location.t
        ; pragma : global_pragma
        }  (** Global directive *)
    | MRecTyp of
        { location : Location.t
        ; declarations : (decl * decl list) List1.t
        }  (** Mutually-recursive LF type family declaration *)
    | Theorems of
        { location : Location.t
        ; theorems : thm_decl List1.t
        }  (** Mutually recursive theorem declaration(s) *)
    | Val of
        { location : Location.t
        ; identifier : Name.t
        ; typ : Comp.typ option
        ; expression : Comp.exp
        }  (** Computation-level value declaration *)
    | Query of
        { location : Location.t
        ; name : Name.t option
        ; mctx : CLF.mctx
        ; typ : CLF.typ
        ; expected_solutions : int option
        ; maximum_tries : int option
        }  (** Logic programming query on LF type *)
    | MQuery of
        { location : Location.t
        ; typ : Comp.typ
        ; expected_solutions : int option
        ; search_tries : int option
        ; search_depth : int option
        }  (** Logic programming mquery on Comp. type *)
    | Module of
        { location : Location.t
        ; identifier : string
        ; declarations : decl list
        }  (** Namespace declaration for other declarations *)
    | Comment of
        { location : Location.t
        ; content : string
        }  (** Documentation comment *)

  (** Parsed Beluga project *)
  type sgn = decl list
end