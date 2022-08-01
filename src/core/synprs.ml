(* Parser Syntax *)

open Support

(** Parser LF Syntax *)
module LF = struct
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

  and sch_elem =
    | SchElem of Location.t * typ_decl ctx * typ_rec

  and schema =
    | Schema of sch_elem list

  and mctx = ctyp_decl ctx

  type mfront =
    | ClObj of dctx * sub
    (* ClObj doesn't *really* contain just a substitution.
       The problem is that syntactically, we can't tell
       whether `[psi |- a]' is a boxed object or
       substitution! So it turns out that,
       syntactically, substitutions encompass both
       possibilities: a substitution beginning with
       EmptySub and having just one term term in it
       can represent a boxed term. We disambiguate
       substitutions from terms at a later time. *)
    | CObj of dctx

  let loc_of_head =
    function
    | Name (l, _, _) -> l
    | Hole l -> l
    | PVar (l, _, _) -> l
    | Proj (l, _, _) -> l
end


(** Parser Computation Syntax *)
module Comp = struct
  include Syncom.Comp

  type kind =
    | Ctype of { location : Location.t }
    | ArrKind of
        { location : Location.t
        ; domain : LF.ctyp
        ; range : kind
        }
    | PiKind of
        { location : Location.t
        ; parameter_name : Name.t
        ; parameter_type : LF.ctyp
        ; plicity : Plicity.t
        ; range : kind
        }

  type meta_obj = Location.t * LF.mfront

  type meta_typ = LF.ctyp

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
        ; parameter_type : LF.ctyp
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
        ; branches : fun_branches
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
        ; variable_name : Name.t
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
    | PatName of
        { location : Location.t
        ; name : Name.t
        ; spine : pattern_spine
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

  and pattern_spine =
    [ `PatApp of Location.t * pattern
    | `PatObs of Location.t * Name.t
    ]
    list

  and branch =
    | Branch of Location.t * LF.ctyp_decl LF.ctx * pattern * exp

  and fun_branches =
    | NilFBranch of Location.t
    | ConsFBranch of Location.t * (pattern_spine * exp) * fun_branches

  type suffices_typ = typ generic_suffices_typ

  type named_order = Name.t generic_order
  type numeric_order = int generic_order

  type total_dec =
    | NumericTotal of Location.t * numeric_order option
    | NamedTotal of Location.t * named_order option * Name.t * Name.t option list
    | Trust of Location.t

  type ctyp_decl =
    | CTypDecl of Name.t * typ

  type gctx = ctyp_decl LF.ctx

  type hypotheses =
    { cD : LF.mctx
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

(** Syntax of Harpoon commands. *)
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

  type info_kind =
    [ `prog
    ]

  type command =
    (* Administrative commands *)
    | Rename of
      { rename_from: Name.t
      ; rename_to: Name.t
      ; level: level
      }
    | ToggleAutomation of automation_kind * automation_change

    | Type of Comp.exp
    | Info of info_kind * Name.t
    | SelectTheorem of Name.t
    | Theorem of [ basic_command | `show_ihs | `show_proof | `dump_proof of string ]
    | Session of [ basic_command | `create | `serialize ]
    | Subgoal of basic_command
    | Undo
    | Redo
    | History

    | Translate of Name.t

    (* Actual tactics *)
    | Intros of string list option (* list of names for introduced variables *)

    | Split of split_kind * Comp.exp (* the expression to split on *)
    | MSplit of Location.t * Name.t (* split on a metavariable *)
    | Solve of Comp.exp (* the expression to solve the current subgoal with *)
    | Unbox of Comp.exp * Name.t * Comp.unbox_modifier option
    | By of Comp.exp * Name.t
    | Suffices of Comp.exp * Comp.suffices_typ list
    | Help
end


(** Parser Signature Syntax *)
module Sgn = struct

  type datatype_flavour =
    | InductiveDatatype
    | StratifiedDatatype

  type precedence = int

  type pragma =
    | OptsPrag          of string list
    | NamePrag          of Name.t * string * string option
    | FixPrag           of Name.t * Fixity.t * precedence * Associativity.t option
    | NotPrag
    | DefaultAssocPrag  of Associativity.t
    | OpenPrag          of string list
    | AbbrevPrag        of string list * string

  (* Pragmas that need to be declared first *)
  type global_pragma =
    | NoStrengthen
    | Coverage of [`Error | `Warn]

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
      { location: Location.t
      ; identifier: Name.t
      ; kind: LF.kind
      } (** LF type family declaration *)

    | Const of
      { location: Location.t
      ; identifier: Name.t
      ; typ: LF.typ
      } (** LF type constant decalaration *)

    | CompTyp of
      { location: Location.t
      ; identifier: Name.t
      ; kind: Comp.kind
      ; datatype_flavour: datatype_flavour
      } (** Computation-level data type constant declaration *)

    | CompCotyp of
      { location: Location.t
      ; identifier: Name.t
      ; kind: Comp.kind
      } (** Computation-level codata type constant declaration *)

    | CompConst of
      { location: Location.t
      ; identifier: Name.t
      ; typ: Comp.typ
      } (** Computation-level type constructor declaration *)

    | CompDest of
      { location: Location.t
      ; identifier: Name.t
      ; mctx: LF.mctx
      ; observation_typ: Comp.typ
      ; return_typ: Comp.typ
      } (** Computation-level type destructor declaration *)

    | CompTypAbbrev of
      { location: Location.t
      ; identifier: Name.t
      ; kind: Comp.kind
      ; typ: Comp.typ
      } (** Synonym declaration for computation-level type *)

    | Schema of
      { location: Location.t
      ; identifier: Name.t
      ; schema: LF.schema
      } (** Declaration of a specification for a set of contexts *)

    | Pragma of
      { location: Location.t
      ; pragma: pragma
      } (** Compiler directive *)

    | GlobalPragma of
      { location: Location.t
      ; pragma: global_pragma
      } (** Global directive *)

    | MRecTyp of
      { location: Location.t
      ; declarations: (decl * decl list) List1.t
      } (** Mutually-recursive LF type family declaration *)

    | Theorems of
      { location: Location.t
      ; theorems: thm_decl List1.t
      } (** Mutually recursive theorem declaration(s) *)

    | Val of
      { location: Location.t
      ; identifier: Name.t
      ; typ: Comp.typ option
      ; expression: Comp.exp
      } (** Computation-level value declaration *)

    | Query of
      { location: Location.t
      ; name: Name.t option
      ; mctx: LF.mctx
      ; typ: LF.typ
      ; expected_solutions: int option
      ; maximum_tries: int option
      } (** Logic programming query on LF type *)

    | MQuery of
      { location: Location.t
      ; typ: Comp.typ
      ; expected_solutions: int option
      ; search_tries: int option
      ; search_depth: int option
      } (** Logic programming mquery on Comp. type *)

    | Module of
      { location: Location.t
      ; identifier: string
      ; declarations: decl list
      } (** Namespace declaration for other declarations *)

    | Comment of
      { location: Location.t
      ; content: string
      } (** Documentation comment *)

  (** Parsed Beluga project *)
  type sgn = decl list

end
