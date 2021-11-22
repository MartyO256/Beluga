open Support
open Common

type unbox_modifier = [ `strengthened ]
type case_pragma = PragmaCase | PragmaNotCase

type context_case =
  | EmptyContext of Location.t
  | ExtendedBy of Location.t * int (* specifies a schema element *)

type case_label =
  | NamedCase of Location.t * Name.t
  | BVarCase of Location.t
  | ContextCase of context_case
  | PVarCase of
      Location.t
      * int (* schema element number (1-based) *)
      * int option
(* the number of the projection, if any (1-based) *)

type 'a generic_order =
  | Arg of 'a                       (* O ::= x                    *)
  | Lex of 'a generic_order list    (*     | {O1 .. On}           *)
  | Simul of 'a generic_order list  (*     | [O1 .. On]           *)
(* Note: Simul is currently unused. It doesn't even have a parser. -je *)

type 'a generic_suffices_typ =
  [ `exact of 'a (* user specified an exact type annotation *)
  | `infer of
    Location.t
    (* user specified `_` and expects the type to be known *)
  ]
(** Type specified in an interactive use of `suffices` *)

let map_suffices_typ (f : 'a -> 'b) :
    'a generic_suffices_typ -> 'b generic_suffices_typ =
  function
  | `exact x -> `exact (f x)
  | `infer loc -> `infer loc

let rec map_order (f : 'a -> 'b) :
    'a generic_order -> 'b generic_order = function
  | Arg x -> Arg (f x)
  | Lex xs -> Lex (List.map (map_order f) xs)
  | Simul xs -> Simul (List.map (map_order f) xs)

type kind =
  | Ctype of Location.t
  | PiKind of Location.t * LF.ctyp_decl * kind

type meta_obj = Location.t * LF.mfront

type meta_spine =
                                      (* Meta-Spine  mS :=         *)
  | MetaNil                           (* | .                       *)
  | MetaApp of meta_obj * meta_spine  (* | mC mS                   *)

type meta_typ = LF.loc_ctyp

(** Computation-level types. *)
type typ =
  | TypBase of Location.t * Name.t * meta_spine (*    | c mS               *)
  | TypBox of Location.t * meta_typ             (*    | [U]                *)
  | TypArr of Location.t * typ * typ            (*    | tau -> tau         *)
  | TypCross of Location.t * typ * typ          (*    | tau * tau          *)
  | TypPiBox of Location.t * LF.ctyp_decl * typ (*    | Pi u::U.tau        *)
  | TypInd of typ

(** Computation-level expressions. *)
and exp_chk =
  | Syn of Location.t * exp_syn                                   (*  e ::= i                      *)
  | Fn of Location.t * Name.t * exp_chk                           (*    | fn x => e                *)
  | Fun of Location.t * fun_branches                              (*    | fun fbranches            *)
  | MLam of Location.t * Name.t * exp_chk                         (*    | mlam f => e              *)
  | Pair of Location.t * exp_chk * exp_chk                        (*    | (e1 , e2)                *)
  | LetPair of Location.t * exp_syn * (Name.t * Name.t * exp_chk) (*    | let (x,y) = i in e       *)
  | Let of Location.t * exp_syn * (Name.t * exp_chk)              (*    | let x = i in e           *)
  | Box of Location.t * meta_obj                                  (*    | [C]                      *)
  | Impossible of Location.t * exp_syn                            (*    | impossible i             *)
  | Case of Location.t * case_pragma * exp_syn * branch list      (*    | case i of branches       *)
  | Hole of Location.t * string option                            (*    | ?name                    *)
  | BoxHole of Location.t                                         (*    | _                        *)

and exp_syn =
  | Name of Location.t * FullyQualifiedName.t (*  i ::= x/c               *)
  | Apply of Location.t * exp_syn * exp_chk   (*    | i e                 *)
  | BoxVal of Location.t * meta_obj           (*    | [C]                 *)
  | PairVal of Location.t * exp_syn * exp_syn (*    | (i , i)             *)

(* Note that observations are missing.
   In the external syntax, observations are syntactically
   indistinguishable from applications, so we parse them as
   applications. During indexing, they are disambiguated into
   observations. *)
and pattern =
  | PatMetaObj of Location.t * meta_obj
  | PatName of Location.t * Name.t * pattern_spine
  | PatPair of Location.t * pattern * pattern
  | PatAnn of Location.t * pattern * typ

and pattern_spine =
  | PatNil of Location.t
  | PatApp of Location.t * pattern * pattern_spine
  | PatObs of Location.t * Name.t * pattern_spine

and branch =
  | Branch of
      Location.t * LF.ctyp_decl LF.ctx * pattern * exp_chk

and fun_branches =
  | NilFBranch of Location.t
  | ConsFBranch of
      Location.t * (pattern_spine * exp_chk) * fun_branches

(* the definition of branch_pattern will be removed and replaced by the more general notion of patterns;
   it remains currently so we can still use the old parser without modifications -bp *)
and branch_pattern =
  | NormalPattern of LF.normal * exp_chk
  | EmptyPattern

type suffices_typ = typ generic_suffices_typ
type named_order = Name.t generic_order
type numeric_order = int generic_order

type total_dec =
  | NumericTotal of Location.t * numeric_order option
  | NamedTotal of
      Location.t
      * named_order option
      * Name.t
      * Name.t option list
  | Trust of Location.t

type ctyp_decl = CTypDecl of Name.t * typ
type gctx = ctyp_decl LF.ctx
type hypotheses = { cD : LF.mctx; cG : gctx }

type proof =
  | Incomplete of Location.t * string option
  | Command of Location.t * command * proof
  | Directive of Location.t * directive

and command =
  | By of Location.t * exp_syn * Name.t
  | Unbox of Location.t * exp_syn * Name.t * unbox_modifier option

and directive =
  | Intros of Location.t * hypothetical
  | Solve of Location.t * exp_chk
  | Split of Location.t * exp_syn * split_branch list
  | Suffices of Location.t * exp_syn * (Location.t * typ * proof) list

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

type thm = Program of exp_chk | Proof of proof
