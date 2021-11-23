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
  | Typ of Location.t
  | ArrKind of Location.t * typ * kind
  | PiKind of Location.t * typ_decl * kind

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

and loc_ctyp = Location.t * ctyp

and ctyp_decl =
  | Decl of Name.t * loc_ctyp * Depend.t
  | DeclAnonymous of loc_ctyp * Depend.t
  | DeclOpt of Name.t
(* x : _ *)

and typ =
  | Atom of Location.t * Name.t * spine
  | ArrTyp of Location.t * typ * typ
  | PiTyp of Location.t * typ_decl * typ
  | Sigma of Location.t * typ_rec
  | Ctx of Location.t * dctx
  | AtomTerm of Location.t * normal

and normal =
  | Lam of Location.t * Name.t * normal
  | Root of Location.t * head * spine
  | Tuple of Location.t * tuple
  | LFHole of Location.t * string option
  | Ann of Location.t * normal * typ
  | TList of Location.t * normal list
  | NTyp of Location.t * typ
  | PatEmpty of Location.t

and head =
  | Name of Location.t * FullyQualifiedName.t * sub option
  | Hole of Location.t
  | PVar of Location.t * Name.t * sub option
  | Proj of Location.t * head * proj

and proj =
  | ByPos of int
  | ByName of Name.t

and spine =
  | Nil
  | App of Location.t * normal * spine

and sub_start =
  | EmptySub of Location.t
  | Id of Location.t
  | SVar of Location.t * Name.t * sub option

and sub = sub_start * normal list

and typ_rec =
  | SigmaLast of Name.t option * typ
  | SigmaElem of Name.t * typ * typ_rec

and tuple =
  | Last of normal
  | Cons of normal * tuple

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
let rec list_of_spine : spine -> (Location.t * normal) list = function
  | Nil ->
      []
  | App (l, m, s) ->
      (l, m) :: list_of_spine s


let loc_of_normal = function
  | Lam (l, _, _) ->
      l
  | Root (l, _, _) ->
      l
  | Tuple (l, _) ->
      l
  | LFHole (l, _) ->
      l
  | Ann (l, _, _) ->
      l
  | TList (l, _) ->
      l
  | NTyp (l, _) ->
      l
  | PatEmpty l ->
      l


let loc_of_head = function
  | Name (l, _, _) ->
      l
  | Hole l ->
      l
  | PVar (l, _, _) ->
      l
  | Proj (l, _, _) ->
      l


(** Wraps a term into a dummy substitution. *)
let term tM = (EmptySub (loc_of_normal tM), [ tM ])
