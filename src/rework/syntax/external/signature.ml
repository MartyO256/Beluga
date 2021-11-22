open Support
open Common

type datatype_flavour =
  | InductiveDatatype
  | StratifiedDatatype

type assoc =
  | Left
  | Right
  | None

type precedence = int

type fix =
  | Prefix
  | Postfix
  | Infix

type pragma =
  | OptsPrag of string list
  | NamePrag of Name.t * string * string option
  | FixPrag of Name.t * fix * precedence * assoc option
  | NotPrag
  | DefaultAssocPrag of assoc
  | OpenPrag of string list
  | AbbrevPrag of string list * string

(* Pragmas that need to be declared first *)
type global_pragma =
  | NoStrengthen
  | Coverage of [ `Error | `Warn ]

type thm_decl =
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
      ; kind : LF.kind
      } (** LF type family declaration *)
  | Const of
      { location : Location.t
      ; identifier : Name.t
      ; typ : LF.typ
      } (** LF type constant decalaration *)
  | CompTyp of
      { location : Location.t
      ; identifier : Name.t
      ; kind : Comp.kind
      ; datatype_flavour : datatype_flavour
      } (** Computation-level data type constant declaration *)
  | CompCotyp of
      { location : Location.t
      ; identifier : Name.t
      ; kind : Comp.kind
      } (** Computation-level codata type constant declaration *)
  | CompConst of
      { location : Location.t
      ; identifier : Name.t
      ; typ : Comp.typ
      } (** Computation-level type constructor declaration *)
  | CompDest of
      { location : Location.t
      ; identifier : Name.t
      ; mctx : LF.mctx
      ; observation_typ : Comp.typ
      ; return_typ : Comp.typ
      } (** Computation-level type destructor declaration *)
  | CompTypAbbrev of
      { location : Location.t
      ; identifier : Name.t
      ; kind : Comp.kind
      ; typ : Comp.typ
      } (** Synonym declaration for computation-level type *)
  | Schema of
      { location : Location.t
      ; identifier : Name.t
      ; schema : LF.schema
      } (** Declaration of a specification for a set of contexts *)
  | Pragma of
      { location : Location.t
      ; pragma : pragma
      } (** Compiler directive *)
  | GlobalPragma of
      { location : Location.t
      ; pragma : global_pragma
      } (** Global directive *)
  | MRecTyp of
      { location : Location.t
      ; declarations : (decl * decl list) Nonempty.t
      } (** Mutually-recursive LF type family declaration *)
  | Theorem of
      { location : Location.t
      ; theorems : thm_decl Nonempty.t
      } (** Mutually recursive theorem declaration(s) *)
  | Val of
      { location : Location.t
      ; identifier : Name.t
      ; typ : Comp.typ option
      ; expression : Comp.exp_syn
      } (** Computation-level value declaration *)
  | Query of
      { location : Location.t
      ; name : Name.t option
      ; typ : LF.typ
      ; expected_solutions : int option
      ; maximum_tries : int option
      } (** Logic programming query on LF type *)
  | Module of
      { location : Location.t
      ; identifier : Name.t
      ; declarations : decl list
      } (** Namespace declaration for other declarations *)
  | Comment of
      { location : Location.t
      ; content : string
      } (** Documentation comment *)

(** Parsed Beluga project *)
type sgn = decl list
