open Support
open Beluga
open Common

type offset = int

module LF = struct
  type 'a ctx = 'a list

  type svar_class =
    | Ren
    | Subst

  type kind =
    | Typ
    | PiKind of (typ_decl * Depend.t) * kind

  and typ_decl =
    | TypDecl of Name.t * typ
    | TypDeclOpt of Name.t

  and cltyp =
    | MTyp of typ
    | PTyp of typ
    | STyp of svar_class * dctx

  and ctyp =
    | ClTyp of cltyp * dctx
    | CTyp of Id.Schema.t Option.t

  and ctyp_decl =
    | Decl of Name.t * ctyp * Depend.t
    | DeclOpt of Name.t * Plicity.t

  and typ =
    | Atom of Location.t * Id.Typ.t * spine
    | PiTyp of (typ_decl * Depend.t) * typ
    | Sigma of typ_rec
    | TClo of typ * sub

  and normal =
    | Lam of Location.t * Name.t * normal
    | Root of Location.t * head * spine * Plicity.t
    | LFHole of Location.t * HoleId.t * HoleId.name
    | Clo of normal * sub
    | Tuple of Location.t * tuple

  and head =
    | BVar of int
    | Const of Id.Const.t
    | MMVar of mm_var_inst
    | MPVar of mm_var_inst
    | MVar of cvar * sub
    | PVar of offset * sub
    | AnnH of head * typ
    | Proj of head * int
    | FVar of Name.t
    | FMVar of fvarsub
    | FPVar of fvarsub
    | HClo of offset * offset * sub
    | HMClo of offset * mm_var_inst

  and fvarsub = Name.t * sub

  and spine =
    | Nil
    | App of normal * spine
    | SClo of spine * sub

  and sub =
    | Shift of offset
    | SVar of offset * int * sub
    | FSVar of offset * fvarsub
    | Dot of front * sub
    | MSVar of offset * mm_var_inst
    | EmptySub
    | Undefs

  and front =
    | Head of head
    | Obj of normal
    | Undef

  and mfront =
    | ClObj of dctx_hat * clobj
    | CObj of dctx
    | MV of offset
    | MUndef

  and clobj =
    | MObj of normal
    | PObj of head
    | SObj of sub

  and msub =
    | MShift of int
    | MDot of mfront * msub

  and cvar =
    | Offset of offset
    | Inst of mm_var

  and mm_var =
    { name : Name.t
    ; instantiation : iterm option ref
    ; cD : mctx
    ; mmvar_id : int
    ; typ : ctyp
    ; constraints : cnstr list ref
    ; depend : Depend.t
    }

  and mm_var_inst' = mm_var * msub

  and mm_var_inst = mm_var_inst' * sub

  and iterm =
    | INorm of normal
    | IHead of head
    | ISub of sub
    | ICtx of dctx

  and tvar = TInst of typ option ref * dctx * kind * cnstr list ref

  and typ_free_var =
    | Type of typ
    | TypVar of tvar

  and constrnt_id = int

  and constrnt =
    | Queued of constrnt_id
    | Eqn of constrnt_id * mctx * dctx * iterm * iterm

  and cnstr = constrnt ref

  and dctx =
    | Null
    | CtxVar of ctx_var
    | DDec of dctx * typ_decl

  and ctx_var =
    | CtxName of Name.t
    | CtxOffset of offset
    | CInst of mm_var_inst'

  and sch_elem = SchElem of typ_decl ctx * typ_rec

  and schema = Schema of sch_elem list

  and dctx_hat = ctx_var option * offset

  and typ_rec =
    | SigmaLast of Name.t option * typ
    | SigmaElem of Name.t * typ * typ_rec

  and tuple =
    | Last of normal
    | Cons of normal * tuple

  and mctx = ctyp_decl ctx

  type nclo = normal * sub

  type sclo = spine * sub

  type tclo = typ * sub

  type trec_clo = typ_rec * sub

  type assoc =
    | Left
    | Right
    | NoAssoc

  type fix =
    | Prefix
    | Postfix
    | Infix

  type prag =
    | NamePrag of Id.Typ.t
    | NotPrag
    | OpenPrag of Id.Module.t
    | DefaultAssocPrag of assoc
    | FixPrag of Name.t * fix * int * assoc option
    | AbbrevPrag of string list * string
end

module Comp = struct
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

  type unbox_modifier = [ `strengthened ]

  type case_pragma =
    | PragmaCase
    | PragmaNotCase

  type context_case =
    | EmptyContext of Location.t
    | ExtendedBy of Location.t * int

  type case_label =
    | NamedCase of Location.t * Name.t
    | BVarCase of Location.t
    | ContextCase of context_case
    | PVarCase of Location.t * int * int option

  type 'a generic_order =
    | Arg of 'a
    | Lex of 'a generic_order list
    | Simul of 'a generic_order list

  type 'a generic_suffices_typ =
    [ `exact of 'a
    | `infer of Location.t
    ]

  type kind =
    | Ctype of Location.t
    | PiKind of Location.t * LF.ctyp_decl * kind

  type meta_typ = LF.ctyp

  type meta_obj = Location.t * LF.mfront

  type meta_spine =
    | MetaNil
    | MetaApp of meta_obj * meta_typ * meta_spine * Plicity.t

  type typ =
    | TypBase of Location.t * Id.CompTyp.t * meta_spine
    | TypCobase of Location.t * Id.CompCotyp.t * meta_spine
    | TypDef of Location.t * Id.Comp.t * meta_spine
    | TypBox of Location.t * meta_typ
    | TypArr of Location.t * typ * typ
    | TypCross of Location.t * typ * typ
    | TypPiBox of Location.t * LF.ctyp_decl * typ
    | TypClo of typ * LF.msub
    | TypInd of typ

  type suffices_typ = typ generic_suffices_typ

  type ih_arg =
    | M of meta_obj
    | V of offset
    | E
    | DC

  type wf_tag = bool

  type ctyp_decl =
    | CTypDecl of Name.t * typ * wf_tag
    | CTypDeclOpt of Name.t

  type ih_decl = WfRec of Name.t * ih_arg list * typ

  type gctx = ctyp_decl LF.ctx

  type ihctx = ih_decl LF.ctx

  and exp_chk =
    | Syn of Location.t * exp_syn
    | Fn of Location.t * Name.t * exp_chk
    | Fun of Location.t * fun_branches
    | MLam of Location.t * Name.t * exp_chk * Plicity.t
    | Pair of Location.t * exp_chk * exp_chk
    | LetPair of Location.t * exp_syn * (Name.t * Name.t * exp_chk)
    | Let of Location.t * exp_syn * (Name.t * exp_chk)
    | Box of Location.t * meta_obj * meta_typ
    | Case of Location.t * case_pragma * exp_syn * branch list
    | Impossible of Location.t * exp_syn
    | Hole of Location.t * HoleId.t * HoleId.name

  and exp_syn =
    | Var of Location.t * offset
    | DataConst of Location.t * Id.CompConst.t
    | Obs of Location.t * exp_chk * LF.msub * Id.CompDest.t
    | Const of Location.t * Id.Comp.t
    | Apply of Location.t * exp_syn * exp_chk
    | MApp of Location.t * exp_syn * meta_obj * meta_typ * Plicity.t
    | AnnBox of meta_obj * meta_typ
    | PairVal of Location.t * exp_syn * exp_syn

  and pattern =
    | PatMetaObj of Location.t * meta_obj
    | PatConst of Location.t * Id.CompConst.t * pattern_spine
    | PatFVar of Location.t * Name.t
    | PatVar of Location.t * offset
    | PatPair of Location.t * pattern * pattern
    | PatAnn of Location.t * pattern * typ * Plicity.t

  and pattern_spine =
    | PatNil
    | PatApp of Location.t * pattern * pattern_spine
    | PatObs of Location.t * Id.CompDest.t * LF.msub * pattern_spine

  and branch =
    | Branch of
        Location.t * LF.mctx * (LF.mctx * gctx) * pattern * LF.msub * exp_chk

  and fun_branches =
    | NilFBranch of Location.t
    | ConsFBranch of
        Location.t
        * (LF.mctx * gctx * pattern_spine * exp_chk)
        * fun_branches

  type tclo = typ * LF.msub

  type order = int generic_order

  type 'order total_dec_kind =
    [ `inductive of 'order
    | `not_recursive
    | `trust
    | `partial
    ]

  type total_dec =
    { name : Name.t
    ; tau : typ
    ; order : order total_dec_kind
    }

  type hypotheses =
    { cD : LF.mctx
    ; cG : gctx
    ; cIH : ihctx
    }

  type meta_branch_label =
    [ `ctor of Id.Const.t
    | `pvar of int option
    | `bvar
    ]

  module SubgoalPath = struct
    type t =
      | Here
      | Intros of t
      | Suffices of exp_syn * int * t
      | MetaSplit of exp_syn * meta_branch_label * t
      | CompSplit of exp_syn * Id.CompConst.t * t
      | ContextSplit of exp_syn * context_case * t

    type builder = t -> t
  end

  type proof =
    | Incomplete of Location.t * proof_state
    | Command of command * proof
    | Directive of directive

  and command =
    | By of exp_syn * Name.t * typ
    | Unbox of exp_syn * Name.t * LF.ctyp * unbox_modifier option

  and proof_state =
    { context : hypotheses
    ; label : SubgoalPath.builder
    ; goal : tclo
    ; solution : proof option ref
    }

  and directive =
    | Intros of hypothetical
    | Solve of exp_chk
    | ImpossibleSplit of exp_syn
    | Suffices of exp_syn * suffices_arg list
    | MetaSplit of exp_syn * typ * meta_branch list
    | CompSplit of exp_syn * typ * comp_branch list
    | ContextSplit of exp_syn * typ * context_branch list

  and suffices_arg = Location.t * typ * proof

  and context_branch = context_case split_branch

  and meta_branch = meta_branch_label split_branch

  and comp_branch = Id.CompConst.t split_branch

  and 'b split_branch =
    | SplitBranch of 'b * (gctx * pattern) * LF.msub * hypothetical

  and hypothetical = Hypothetical of Location.t * hypotheses * proof

  type open_subgoal = Id.CompConst.t * proof_state

  type thm =
    | Proof of proof
    | Program of exp_chk

  type env =
    | Empty
    | Cons of value * env

  and value =
    | FnValue of Name.t * exp_chk * LF.msub * env
    | FunValue of fun_branches_value
    | ThmValue of Id.Comp.t * thm * LF.msub * env
    | MLamValue of Name.t * exp_chk * LF.msub * env
    | CtxValue of Name.t * exp_chk * LF.msub * env
    | BoxValue of meta_obj
    | ConstValue of Id.Comp.t
    | DataValue of Id.CompConst.t * data_spine
    | PairValue of value * value

  and data_spine =
    | DataNil
    | DataApp of value * data_spine

  and fun_branches_value =
    | NilValBranch
    | ConsValBranch of
        (pattern_spine * exp_chk * LF.msub * env) * fun_branches_value
end

module Sgn = struct
  type positivity_flag =
    | Nocheck
    | Positivity
    | Stratify of Location.t * int
    | StratifyAll of Location.t

  type thm_decl =
    { thm_name : Id.Comp.t
    ; thm_typ : Comp.typ
    ; thm_body : Comp.thm
    ; thm_loc : Location.t
    }
end
