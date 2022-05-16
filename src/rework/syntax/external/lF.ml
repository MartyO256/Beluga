module L = Location
open Support
open Common

type 'a ctx =
  (* Generic context declaration    *)
  | Empty (* C ::= Empty                    *)
  | Dec of 'a ctx * 'a
(* | C, x:'a                      *)

(** Substitution variable class *)
type svar_class =
  | Ren  (** Renaming *)
  | Subst  (** Substitution *)

type kind =
  | Typ of L.t
  | ArrKind of L.t * typ * kind
  | PiKind of L.t * typ_decl * kind

and typ_decl =
  | TypDecl of Name.t * typ
  | TypDeclOpt of Name.t

and cltyp =
  | MTyp of typ
  | PTyp of typ
  | STyp of svar_class * dctx

and ctyp =
  | ClTyp of cltyp * dctx
  | CTyp of FullyQualifiedName.t

and loc_ctyp = L.t * ctyp

and ctyp_decl =
  | Decl of Name.t * loc_ctyp * Depend.t
  | DeclAnonymous of loc_ctyp * Depend.t
  | DeclOpt of Name.t
(* x : _ *)

and typ =
  | Atom of L.t * Name.t * spine
  | ArrTyp of L.t * typ * typ
  | PiTyp of L.t * typ_decl * typ
  | Sigma of L.t * typ_rec
  | Ctx of L.t * dctx
  | AtomTerm of L.t * normal

and normal =
  | Lam of L.t * Name.t * normal
  | Root of L.t * head * spine
  | Tuple of L.t * tuple
  | LFHole of L.t * string option
  | Ann of L.t * normal * typ
  | TList of L.t * normal list
  | NTyp of L.t * typ
  | PatEmpty of L.t

and head =
  | Name of L.t * FullyQualifiedName.t * sub option
  | Hole of L.t
  | PVar of L.t * Name.t * sub option
  | Proj of L.t * head * proj

and proj =
  | ByPos of int
  | ByName of Name.t

and spine =
  | Nil
  | App of L.t * normal * spine

and sub_start =
  | EmptySub of L.t
  | Id of L.t
  | SVar of L.t * Name.t * sub option

and sub = sub_start * normal list

and typ_rec =
  | SigmaLast of Name.t option * typ
  | SigmaElem of Name.t * typ * typ_rec

and tuple = normal List1.t

and dctx =
  | Null
  | CtxVar of L.t * Name.t
  | DDec of dctx * typ_decl
  | CtxHole

and sch_elem = SchElem of L.t * typ_decl ctx * typ_rec

and schema = Schema of sch_elem list

and mctx = ctyp_decl ctx

type mfront =
  | ClObj of dctx * sub
  (* ClObj doesn't *really* contain just a substitution.
     The problem is that syntactically, we can't tell
     whether `[psi |- a]' is a boxed object or
     substitution! So it turns out that,
     syntactically, substitutions encompass both
     possibilities: a substitution beginning with
     EmptySub and having just one normal term in it
     can represent a boxed term. We disambiguate
     substitutions from terms at a later time. *)
  | CObj of dctx

(** Converts a spine to a list. It is visually "backwards" *)
let rec list_of_spine : spine -> (L.t * normal) list = function
  | Nil ->
      []
  | App (l, m, s) ->
      (l, m) :: list_of_spine s


let loc_of_normal = function
  | Lam (l, _, _)
  | Root (l, _, _)
  | Tuple (l, _)
  | LFHole (l, _)
  | Ann (l, _, _)
  | TList (l, _)
  | NTyp (l, _)
  | PatEmpty l ->
      l


let loc_of_head = function
  | Name (l, _, _) | Hole l | PVar (l, _, _) | Proj (l, _, _) ->
      l


(** Wraps a term into a dummy substitution. *)
let term tM = (EmptySub (loc_of_normal tM), [ tM ])
