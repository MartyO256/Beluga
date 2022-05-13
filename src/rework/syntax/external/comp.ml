module L = Location
open Common

type unbox_modifier = [ `strengthened ]

type case_pragma =
  | PragmaCase
  | PragmaNotCase

type context_case =
  | EmptyContext of L.t
  | ExtendedBy of L.t * int
(* specifies a schema element *)

type case_label =
  | NamedCase of L.t * Name.t
  | BVarCase of L.t
  | ContextCase of context_case
  | PVarCase of
      L.t * int (* schema element number (1-based) *) * int option
(* the number of the projection, if any (1-based) *)

type kind =
  | Ctype of L.t
  | PiKind of L.t * LF.ctyp_decl * kind

type meta_obj = L.t * LF.mfront

type meta_spine =
  (* Meta-Spine  mS :=         *)
  | MetaNil (* | .                       *)
  | MetaApp of meta_obj * meta_spine
(* | mC mS                   *)

type meta_typ = LF.loc_ctyp

(** Computation-level types. *)
type typ =
  | TypBase of L.t * Name.t * meta_spine (*    | c mS               *)
  | TypBox of L.t * meta_typ (*    | [U]                *)
  | TypArr of L.t * typ * typ (*    | tau -> tau         *)
  | TypCross of L.t * typ * typ (*    | tau * tau          *)
  | TypPiBox of L.t * LF.ctyp_decl * typ (*    | Pi u::U.tau        *)
  | TypInd of typ

(** Computation-level expressions. *)
and exp_chk =
  | Syn of L.t * exp_syn (*  e ::= i                      *)
  | Fn of L.t * Name.t * exp_chk (*    | fn x => e                *)
  | Fun of L.t * fun_branches (*    | fun fbranches            *)
  | MLam of L.t * Name.t * exp_chk (*    | mlam f => e              *)
  | Pair of L.t * exp_chk * exp_chk (*    | (e1 , e2)                *)
  | LetPair of L.t * exp_syn * (Name.t * Name.t * exp_chk) (*    | let (x,y) = i in e       *)
  | Let of L.t * exp_syn * (Name.t * exp_chk) (*    | let x = i in e           *)
  | Box of L.t * meta_obj (*    | [C]                      *)
  | Impossible of L.t * exp_syn (*    | impossible i             *)
  | Case of L.t * case_pragma * exp_syn * branch list (*    | case i of branches       *)
  | Hole of L.t * string option (*    | ?name                    *)
  | BoxHole of L.t
(*    | _                        *)

and exp_syn =
  | Name of L.t * FullyQualifiedName.t (*  i ::= x/c               *)
  | Apply of L.t * exp_syn * exp_chk (*    | i e                 *)
  | BoxVal of L.t * meta_obj (*    | [C]                 *)
  | PairVal of L.t * exp_syn * exp_syn
(*    | (i , i)             *)

(* Note that observations are missing.
   In the external syntax, observations are syntactically
   indistinguishable from applications, so we parse them as
   applications. During indexing, they are disambiguated into
   observations. *)
and pattern =
  | PatMetaObj of L.t * meta_obj
  | PatName of L.t * Name.t * pattern_spine
  | PatPair of L.t * pattern * pattern
  | PatAnn of L.t * pattern * typ

and pattern_spine =
  | PatNil of L.t
  | PatApp of L.t * pattern * pattern_spine
  | PatObs of L.t * Name.t * pattern_spine

and branch = Branch of L.t * LF.ctyp_decl LF.ctx * pattern * exp_chk

and fun_branches =
  | NilFBranch of L.t
  | ConsFBranch of L.t * (pattern_spine * exp_chk) * fun_branches

type suffices_typ = typ GenericSufficesTyp.t

type named_order = Name.t GenericOrder.t

type numeric_order = int GenericOrder.t

type total_dec =
  | NumericTotal of L.t * numeric_order option
  | NamedTotal of
      L.t * named_order option * Name.t * Name.t option list
  | Trust of L.t

type ctyp_decl = CTypDecl of Name.t * typ

type gctx = ctyp_decl LF.ctx

type hypotheses =
  { cD : LF.mctx
  ; cG : gctx
  }

type proof =
  | Incomplete of L.t * string option
  | Command of L.t * command * proof
  | Directive of L.t * directive

and command =
  | By of L.t * exp_syn * Name.t
  | Unbox of L.t * exp_syn * Name.t * unbox_modifier option

and directive =
  | Intros of L.t * hypothetical
  | Solve of L.t * exp_chk
  | Split of L.t * exp_syn * split_branch list
  | Suffices of L.t * exp_syn * (L.t * typ * proof) list

and split_branch =
  { case_label : case_label
  ; branch_body : hypothetical
  ; split_branch_loc : L.t
  }

and hypothetical =
  { hypotheses : hypotheses
  ; proof : proof
  ; hypothetical_loc : L.t
  }

type thm =
  | Program of exp_chk
  | Proof of proof
