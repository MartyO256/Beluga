(*
open Support
open Common

type 'a ctx =                        (* Generic context declaration    *)
| Empty                              (* C ::= Empty                    *)
| Dec of 'a ctx * 'a                 (* | C, x:'a                      *)

(** Substitution variable class *)
type svar_class =
  | Ren (** Renaming *)
  | Subst (** Substitution *)

type kind =
  | Typ
  | PiKind of (typ_decl * Depend.t) * kind

and typ_decl =                                (* LF Declarations                *)
  | TypDecl of Name.t * typ                     (* D := x:A                       *)
  | TypDeclOpt of Name.t                        (*   |  x:_                       *)

and cltyp =
  | MTyp of typ
  | PTyp of typ
  | STyp of svar_class * dctx

and ctyp =
  | ClTyp of cltyp * dctx
  | CTyp of
    { schema: Name.t option
    }

and ctyp_decl =                               (* Contextual Declarations        *)
  | Decl of Name.t * ctyp * Depend.t
  | DeclOpt of Name.t * Plicity.t

and typ =                                     (* LF level                       *)
  | Atom of Location.t * cid_typ * spine           (* A ::= a M1 ... Mn              *)
  | PiTyp of (typ_decl * Depend.t) * typ        (*   | Pi x:A.B                   *)
  | Sigma of typ_rec
  | TClo of (typ * sub)                       (*   | TClo(A,s)                  *)

(* The plicity annotation is set to `implicit when reconstructing an
   a hole (_) so that when printing, it can be reproduced correctly.
 *)
and normal =                                       (* normal terms                   *)
  | Lam of Location.t * Name.t * normal              (* M ::= \x.M                     *)
  | Root of Location.t * head * spine * Plicity.t  (*   | h . S                      *)
  | LFHole of Location.t * HoleId.t * HoleId.name
  | Clo of (normal * sub)                          (*   | Clo(N,s)                   *)
  | Tuple of Location.t * tuple

(* TODO: Heads ought to carry their location.
   Erasure currently needs to invent / pretend that a different
   location is the correct one.
 *)
and head =
  | BVar of offset                            (* H ::= x                        *)
  | Const of cid_term                         (*   | c                          *)
  | MMVar of mm_var_inst                      (*   | u[t ; s]                   *)
  | MPVar of mm_var_inst                      (*   | p[t ; s]                   *)
  | MVar of (cvar * sub)                      (*   | u[s]                       *)
  | PVar of (offset * sub)                    (*   | p[s]                       *)
  | AnnH of head * typ                        (*   | (H:A)                      *)
  | Proj of head * int                        (*   | x.k | #p.k s               *)

  | FVar of Name.t                              (* free variable for type
                                                 reconstruction                 *)
  | FMVar of fvarsub                          (* free meta-variable for type
                                                 reconstruction                 *)
  | FPVar of fvarsub                          (* free parameter variable for type
                                                 reconstruction                 *)
  | HClo of offset * offset * sub             (*   | HClo(x, #S[sigma])         *)
  | HMClo of offset * mm_var_inst             (*   | HMClo(x, #S[theta;sigma])  *)

and fvarsub = Name.t * sub
and spine =                                   (* spine                          *)
  | Nil                                       (* S ::= Nil                      *)
  | App of normal * spine                     (*   | M . S                      *)
  | SClo of (spine * sub)                     (*   | SClo(S,s)                  *)

and sub =
  | Shift of offset                           (* sigma ::= ^(psi,n)             *)
  | SVar of offset * int * sub (* BEWARE: offset and int are both ints,
                                   and in the opposite order compared to FSVar and MSVar.
                                   offset is the index into Delta and describes the SVar.
                                   This is a pain to fix *)
  | FSVar of offset * fvarsub                 (*   | s[sigma]                   *)
  | Dot of front * sub                        (*   | Ft . s                     *)
  | MSVar of offset * mm_var_inst             (*   | u[t ; s]                   *)
  | EmptySub
  | Undefs

and front =                                   (* Fronts:                        *)
  | Head of head                              (* Ft ::= H                       *)
  | Obj of normal                             (*    | N                         *)
  | Undef                                     (*    | _                         *)

                                              (* Contextual substitutions       *)
and mfront =                                  (* Fronts:                        *)
  | ClObj of dctx_hat * clobj
  | CObj of dctx                              (*    | Psi                       *)
  | MV of offset                              (*    | u//u | p//p | psi/psi     *)
  | MUndef (* This shouldn't be here, we should use a different datastructure for
             partial inverse substitutions *)

and clobj =                                   (* ContextuaL objects *)
  | MObj of normal                            (* Mft::= Psihat.N                *)
  | PObj of head                              (*    | Psihat.p[s] | Psihat.x    *)
  | SObj of sub

and msub =                                    (* Contextual substitutions       *)
  | MShift of int                             (* theta ::= ^n                   *)
  | MDot of mfront * msub                     (*       | MFt . theta            *)

and cvar =                                    (* Contextual Variables           *)
  | Offset of offset                          (* Bound Variables                *)
  | Inst of mm_var                            (* D ; Psi |- M <= A provided constraint *)

and mm_var =
  { name : Name.t
  ; instantiation : iterm option ref
  ; cD : mctx
  ; mmvar_id : int (* unique to each MMVar *)
  ; typ : ctyp
  ; constraints : cnstr list ref (* not really used *)
  ; depend : Depend.t
  }

and mm_var_inst' = mm_var * msub
and mm_var_inst = mm_var_inst' * sub

and iterm =
  | INorm of normal
  | IHead of head
  | ISub of sub
  | ICtx of dctx

and tvar =
  | TInst of typ option ref * dctx * kind * cnstr list ref

and typ_free_var = Type of typ | TypVar of tvar

(* unique identifiers attached to constraints, used for debugging *)
and constrnt_id = int

and constrnt =                                       (* Constraint                     *)
  | Queued of constrnt_id                            (* constraint ::= Queued          *)
  | Eqn of constrnt_id * mctx * dctx * iterm * iterm (*            | Delta; Psi |-(M1 == M2)  *)

and cnstr = constrnt ref

and dctx =                               (* LF Context                     *)
  | Null                                 (* Psi ::= .                      *)
  | CtxVar of ctx_var                    (* | psi                          *)
  | DDec of dctx * typ_decl              (* | Psi, x:A   or x:block ...    *)

and ctx_var =
  | CtxName of Name.t
  | CtxOffset of offset
  | CInst of mm_var_inst'
     (* D |- Psi : schema   *)

and sch_elem =                           (* Schema Element                 *)
  | SchElem of typ_decl ctx * typ_rec    (* Pi    x1:A1 ... xn:An.
                                            Sigma y1:B1 ... yk:Bk. B       *)
                                         (* Sigma-types not allowed in Ai  *)

and schema =
  | Schema of sch_elem list

and dctx_hat = ctx_var option * offset   (* Psihat ::=         *)
                                         (*        | psi       *)
                                         (*        | .         *)
                                         (*        | Psihat, x *)


and typ_rec =                            (* Sigma x1:A1 ... xn:An. B *)
  |  SigmaLast of Name.t option * typ      (* ... . B *)
  |  SigmaElem of Name.t * typ * typ_rec   (* xk : Ak, ... *)

and tuple =
  | Last of normal
  | Cons of normal * tuple

and mctx = ctyp_decl ctx                 (* Modal Context  D: CDec ctx     *)

let map_plicity f =
  function
  | Root (loc, tH, tS, plicity) ->
     Root (loc, tH, tS, f plicity)
  | tM -> tM

let proj_maybe (h : head) : int option -> head =
  function
  | None -> h
  | Some k -> Proj (h, k)

(** Helper for forming TClo LF types, which avoids doing so if the
    substitution is the identity.
 *)
let tclo tA s =
  match s with
  | Shift 0 -> tA
  | _ -> TClo (tA, s)

(** Forms an MMVar instantiation by attaching an LF substitution and
    a meta-substituation to the variable.
 *)
let mm_var_inst (u : mm_var) (t : msub) (s : sub): mm_var_inst = (u, t), s

let is_mmvar_instantiated mmvar = Option.is_some (mmvar.instantiation.contents)

let rename_ctyp_decl f =
  function
  | Decl (x, tA, ind) -> Decl (f x, tA, ind)
  | DeclOpt (x, plicity) -> DeclOpt (f x, plicity)

(** Embeds a head into a normal by using an empty spine.
    Very useful for constructing variables as normals.
    Note that the normal will have a ghost location, as heads don't
    carry a location.
 *)
let head (tH : head) : normal =
  Root (Location.ghost, tH, Nil, Plicity.explicit)

let mvar cvar sub : head =
  MVar (cvar, sub)

let mmvar inst = MMVar inst

let mpvar inst = MPVar inst

(** Assert that the contextual type declaration be a real Decl, and
    not a DeclOpt.
    Raises a violation if it is a DeclOpt.
 *)
let require_decl : ctyp_decl -> Name.t * ctyp * Depend.t =
  function
  | Decl (u, cU, dep) -> (u, cU, dep)
  | DeclOpt _ ->
     Error.violation "[require_decl] DeclOpt is forbidden"

(* Hatted version of LF.Null *)
let null_hat : dctx_hat = (None, 0)

let rec loc_of_normal =
  function
  | Lam (loc, _, _) -> loc
  | Root (loc, _, _, _) -> loc
  | LFHole (loc, _, _) -> loc
  | Clo (tM, _) -> loc_of_normal tM
  | Tuple (loc, _) -> loc

(**********************)
(* Type Abbreviations *)
(**********************)

type nclo = normal * sub               (* Ns = [s]N                      *)
type sclo = spine * sub                (* Ss = [s]S                      *)
type tclo = typ * sub                  (* As = [s]A                      *)
type trec_clo = typ_rec * sub          (* [s]Arec                        *)

type assoc = Left | Right | NoAssoc
type fix = Prefix | Postfix | Infix
type prag =
  | NamePrag of cid_typ
  | NotPrag
  | OpenPrag of module_id
  | DefaultAssocPrag of assoc
  | FixPrag of name * fix * int * assoc option
  | AbbrevPrag of string list * string

(**********************)
(* Helpers            *)
(**********************)

let rec blockLength =
  function
  | SigmaLast _ -> 1
  | SigmaElem(_x, _tA, recA) -> 1 + blockLength recA

(* getType traverses the typ_rec from left to right;
   target is relative to the remaining suffix of the type

   getType head s_recA target j = (tA, s')

   if  Psi(head) = Sigma recA'
       and [s]recA is a suffix of recA'
   then
       Psi |- [s']tA  <= type

   CLIENTS: pass 1 for the last argument j

  (* getType head s_recA target 1 *)
  val getType : head -> trec_clo -> int -> int -> tclo
*)
let rec getType head s_recA target j =
  match (s_recA, target) with
  | ((SigmaLast (_, lastA), s), 1) ->
      (lastA, s)

  | ((SigmaElem (_x, tA, _recA), s), 1) ->
      (tA, s)

  | ((SigmaElem (_x, _tA, recA), s), target) ->
      let tPj = Proj (head, j) in
        getType head (recA, Dot (Head tPj, s)) (target - 1) (j + 1)

  | _ -> raise Not_found

(* getIndex traverses the typ_rec from left to right;
   target is the name of the projection we're looking for

  Precondition: acc is 1 when the function is 1st called
   acc is an accumulator set to 1 when the function is called

*)
let rec getIndex' trec target acc =
  match trec with
  | SigmaLast(None, _) -> raise Not_found
  | SigmaLast(Some name, _) ->
     if String.compare (string_of_name name) (string_of_name target) == 0
     then acc
     else failwith "Projection Not found"
  | SigmaElem(name, _, trec') ->
     if String.compare (string_of_name name) (string_of_name target) == 0
     then acc
     else getIndex' trec' target (acc + 1)

let getIndex trec target = getIndex' trec target 1

let is_explicit = function
  | Decl(_, _, dep) -> Depend.is_explicit' dep
  | _ -> true

let name_of_ctyp_decl (d : ctyp_decl) =
  match d with
  | Decl (n, _, _) -> n
  | DeclOpt (n, _) -> n

(** Decides whether the given mfront is a variable,
    viz. [projection of a] pattern variable, metavariable, or
    context variable.
    Returns the offset of the variable, and optionally the
    projection offset.
 *)
let variable_of_mfront (mf : mfront) : (offset * offset option) option =
  match mf with
  | ClObj (_, MObj (Root (_, MVar (Offset x,_), _, _)))
    | CObj (CtxVar (CtxOffset x))
    | ClObj (_ , MObj (Root (_,PVar (x,_), _, _)))
    | ClObj (_ , PObj (PVar (x,_)))  ->
     Some (x, None)

  | ClObj (_, MObj (Root (_, Proj (PVar (x, _), k ),_, _)))
    | ClObj (_, PObj (Proj (PVar (x,_), k))) ->
     Some (x, Some k)

  | _ -> None

let get_constraint_id =
  function
  | Eqn (id, _, _, _, _) -> id
  | Queued id -> id

let rec drop_spine k =
  function
  | tS when k = 0 -> tS
  | Nil -> Nil
  | App (_, tS') -> drop_spine (k-1) tS'
 *)
