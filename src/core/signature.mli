(** Beluga signatures.

    @author Marc-Antoine Ouimet *)

open Support

(** Bound variable names.

    These are totally ordered for efficient lookups in map data structures.

    For signatures, a name is typically a string. *)
module Name : sig
  (** The type of names for bound variables. *)
  type t

  (** {1 Collections} *)

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  module Hamt : Hamt.S with type key = t

  module LinkedMap : LinkedMap.S with type key = t

  module LinkedHamt : Support.LinkedHamt.S with type key = t

  module LinkedHamt1 : Support.LinkedHamt.S1 with type key = t

  (** {1 Name Generation} *)

  (** The type of supplier for a name that does not appear in a given set of
      used names. *)
  type fresh_name_supplier = Set.t -> t

  (** [prefixed_fresh_name_supplier base] is the fresh name supplier for
      names prefixed by [base] and optionally having an integer suffix. *)
  val prefixed_fresh_name_supplier : string -> fresh_name_supplier

  (** {1 Instances} *)

  include Eq.EQ with type t := t

  include Ord.ORD with type t := t

  include Show.SHOW with type t := t
end

(** Namespaced bound variable names.

    These are names for referring to bound variable names nested in modules. *)
module QualifiedName : sig
  (** The type of names for referring to names in the current module or in a
      different module. *)
  type t

  (** {1 Constructors} *)

  (** [make ms n] is the qualified name with name [n] when successively
      opening the modules named [ms]. *)
  val make : ?modules:Name.t List.t -> Name.t -> t

  (** {1 Destructors} *)

  (** [name n] is the declaration name referred to by [n]. *)
  val name : t -> Name.t

  (** [modules n] is the list of module names for modules to open to refer to
      [n] in the module opening order. *)
  val modules : t -> Name.t List.t

  (** {1 Instances} *)

  include Show.SHOW with type t := t

  include Eq.EQ with type t := t

  include Ord.ORD with type t := t

  (** {1 Collections} *)

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end

(** Unique identifiers (IDs) for declarations in a signature.

    An ID uniquely refers to a signature declaration in a source file.
    However, since declarations may be elaborated in steps, derived
    declarations share the same ID.

    IDs are generated sequentially using an allocator during signature
    reconstruction. *)
module type ID = sig
  (** The type of identifiers for signature declarations. *)
  type t

  (** {1 Instances} *)

  include Eq.EQ with type t := t

  include Ord.ORD with type t := t

  include Hash.HASH with type t := t

  (** {1 Collections} *)

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  module Hamt : Hamt.S with type key = t
end

(** Beluga declaration identifiers (IDs). *)
module Id : sig
  (** {1 ID Classes} *)

  module Typ : ID

  module Const : ID

  module CompTyp : ID

  module CompCotyp : ID

  module CompConst : ID

  module CompDest : ID

  module Comp : ID

  module Module : ID

  module Query : ID

  module MQuery : ID

  module Schema : ID

  (** The tagged union type of Beluga declaration IDs. *)
  type t =
    [ `Typ_id of Typ.t
    | `Const_id of Const.t
    | `Comp_typ_id of CompTyp.t
    | `Comp_const_id of CompConst.t
    | `Comp_cotyp_id of CompCotyp.t
    | `Comp_dest_id of CompDest.t
    | `Comp_id of Comp.t
    | `Module_id of Module.t
    | `Query_id of Query.t
    | `MQuery_id of MQuery.t
    | `Schema_id of Schema.t
    ]

  (** {1 Constructors} *)

  (** [lift_typ_id id] is [`Typ_id id]. *)
  val lift_typ_id : Typ.t -> t

  (** [lift_const_id id] is [`Const_id id]. *)
  val lift_const_id : Const.t -> t

  (** [lift_comp_typ_id id] is [`Comp_typ_id id]. *)
  val lift_comp_typ_id : CompTyp.t -> t

  (** [lift_comp_const_id id] is [`Comp_const_id id]. *)
  val lift_comp_const_id : CompConst.t -> t

  (** [lift_comp_cotyp_id id] is [`Comp_cotyp_id id]. *)
  val lift_comp_cotyp_id : CompCotyp.t -> t

  (** [lift_comp_dest_id id] is [`Comp_dest_id id]. *)
  val lift_comp_dest_id : CompDest.t -> t

  (** [lift_comp_id id] is [`Comp_id id]. *)
  val lift_comp_id : Comp.t -> t

  (** [lift_module_id id] is [`Module_id id]. *)
  val lift_module_id : Module.t -> t

  (** [lift_query_id id] is [`Query_id id]. *)
  val lift_query_id : Query.t -> t

  (** [lift_mquery_id id] is [`MQuery_id id]. *)
  val lift_mquery_id : MQuery.t -> t

  (** [lift_schema_id id] is [`Schema_id id]. *)
  val lift_schema_id : Schema.t -> t

  (** {1 ID Allocation} *)

  (** Stateful builder pattern for sequentially making distinct IDs. *)
  module Allocator : sig
    (** Instance of the state monad for the integer value of the latest
        allocated ID. *)
    include State.STATE

    (** [initial_state] is the allocator state with [0] as the latest
        allocated ID. *)
    val initial_state : state

    (** {1 ID Builders} *)

    (** [next_typ_id] is an ID allocator whose next ID is an LF type family
        ID. *)
    val next_typ_id : Typ.t t

    (** [next_const_id] is an ID allocator whose next ID is an LF type
        constant ID. *)
    val next_const_id : Const.t t

    (** [next_comp_typ_id] is an ID allocator whose next ID is a
        computation-level data type constant ID. *)
    val next_comp_typ_id : CompTyp.t t

    (** [next_comp_const_id] is an ID allocator whose next ID is a
        computation-level type constructor ID. *)
    val next_comp_const_id : CompConst.t t

    (** [next_comp_cotyp_id] is an ID allocator whose next ID is a
        computation-level codata type constant ID. *)
    val next_comp_cotyp_id : CompCotyp.t t

    (** [next_comp_dest_id] is an ID allocator whose next ID is a
        computation-level type destructor. *)
    val next_comp_dest_id : CompDest.t t

    (** [next_comp_id] is an ID allocator whose next ID is a computation ID. *)
    val next_comp_id : Comp.t t

    (** [next_module_id] is an ID allocator whose next ID is a module ID. *)
    val next_module_id : Module.t t

    (** [next_query_id] is an ID allocator whose next ID is an ID for a logic
        programming query on LF types. *)
    val next_query_id : Query.t t

    (** [next_mquery_id] is an ID allocator whose next ID is an ID for a
        logic programming query on computational types. *)
    val next_mquery_id : MQuery.t t

    (** [next_schema_id] is an ID allocator whose next ID is a schema ID. *)
    val next_schema_id : Schema.t t
  end

  (** {1 Instances} *)

  include Eq.EQ with type t := t

  include Ord.ORD with type t := t

  include Hash.HASH with type t := t

  (** {1 Collections} *)

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  module Hamt : Hamt.S with type key = t
end

module Syntax : sig
  module Int : sig
    type offset = int

    module LF : sig
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

    module Comp : sig
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
            Location.t
            * LF.mctx
            * (LF.mctx * gctx)
            * pattern
            * LF.msub
            * exp_chk

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

      module SubgoalPath : sig
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

    module Sgn : sig
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
  end
end

(** LF type family declarations. *)
module Typ : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make_initial_entry :
       id:Id.Typ.t
    -> name:Name.t
    -> location:Location.t
    -> implicit_arguments:int
    -> LF.kind
    -> t

  (** {1 Destructors} *)

  val id : t -> Id.Typ.t

  val location : t -> Location.t

  val name : t -> Name.t

  val kind : t -> LF.kind

  (** {1 Freezing} *)

  val is_frozen : t -> bool

  val is_unfrozen : t -> bool

  val freeze :
       subordinates:Id.Typ.Set.t
    -> type_subordinated:Id.Typ.Set.t
    -> t
    -> (t, [> `Frozen_typ_declaration_error of Id.Typ.t ]) Result.t

  (** {1 LF Constructors} *)

  val add_constructor :
       Name.t
    -> Id.Const.t
    -> t
    -> ( t
       , [> `Kind_name_collision of Name.t * Id.Const.t * t
         | `Constructor_name_collision of Name.t * Id.Const.t * t
         | `Frozen_typ_declaration_error of Id.Typ.t
         ] )
       result

  val constructors : t -> Id.Const.t Name.Hamt.t

  val has_constructor_with_name : Name.t -> t -> bool

  (** {1 Naming} *)

  val fresh_var_name :
    t -> ?default_base_name:string -> Name.fresh_name_supplier

  val fresh_mvar_name :
    t -> ?default_base_name:string -> Name.fresh_name_supplier

  val set_var_naming_convention : Name.t Option.t -> t -> t

  val set_mvar_naming_convention : Name.t Option.t -> t -> t

  val set_naming_conventions :
    var:Name.t Option.t -> mvar:Name.t Option.t -> t -> t

  (** {1 Subordination} *)

  val is_subordinate :
       t
    -> Id.Typ.t
    -> (bool, [> `Unfrozen_typ_declaration_error of Id.Typ.t ]) Result.t

  val is_type_subordinated :
       t
    -> Id.Typ.t
    -> (bool, [> `Unfrozen_typ_declaration_error of Id.Typ.t ]) Result.t
end

(** LF type constructor declarations. *)
module Const : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make :
       id:Id.Const.t
    -> name:string
    -> location:Location.t
    -> implicit_arguments:int
    -> kind:Id.Typ.t
    -> LF.typ
    -> t

  (** {1 Destructors}*)

  val id : t -> Id.Const.t

  val location : t -> Location.t

  val name : t -> Name.t

  val typ : t -> LF.typ

  val kind : t -> Id.Typ.t
end

(** Computation-level data type constant declarations. *)
module CompTyp : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make_initial_entry :
       id:Id.CompTyp.t
    -> name:Name.t
    -> location:Location.t
    -> implicit_arguments:int
    -> positivity:Sgn.positivity_flag
    -> Comp.kind
    -> t

  (** {1 Destructors} *)

  val id : t -> Id.CompTyp.t

  val location : t -> Location.t

  val name : t -> Name.t

  val kind : t -> Comp.kind

  (** {1 Freezing} *)

  val is_frozen : t -> bool

  val is_unfrozen : t -> bool

  val freeze :
       t
    -> (t, [> `Frozen_comp_typ_declaration_error of Id.CompTyp.t ]) Result.t

  (** {1 Constructors} *)

  val add_constructor :
       Name.t
    -> Id.CompConst.t
    -> t
    -> (t, [> `Frozen_comp_typ_declaration_error of Id.CompTyp.t ]) result

  val constructors : t -> Id.CompConst.t Name.Hamt.t

  val has_constructor_with_name : Name.t -> t -> bool
end

(** Computation-level type constructor declarations. *)
module CompConst : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make :
       id:Id.CompConst.t
    -> name:Name.t
    -> location:Location.t
    -> implicit_arguments:int
    -> kind:Id.CompTyp.t
    -> Comp.typ
    -> t

  (** {1 Destructors}*)

  val id : t -> Id.Const.t

  val location : t -> Location.t

  val name : t -> Name.t

  val implicit_arguments : t -> int

  val typ : t -> Comp.typ

  val kind : t -> Id.CompTyp.t
end

(** Computation-level codata type constant declarations. *)
module CompCotyp : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make_initial_entry :
       id:Id.CompCotyp.t
    -> name:Name.t
    -> location:Location.t
    -> implicit_arguments:int
    -> Comp.kind
    -> t

  (** {1 Destructors} *)

  val id : t -> Id.CompCotyp.t

  val location : t -> Location.t

  val name : t -> Name.t

  val kind : t -> Comp.kind

  (** {1 Freezing} *)

  val is_frozen : t -> bool

  val is_unfrozen : t -> bool

  val freeze :
       t
    -> ( t
       , [> `Frozen_comp_cotyp_declaration_error of Id.CompCotyp.t ] )
       Result.t

  (** {1 Destructors} *)

  val add_destructor :
       Name.t
    -> Id.CompDest.t
    -> t
    -> ( t
       , [> `Frozen_comp_cotyp_declaration_error of Id.CompCotyp.t ] )
       result

  val destructors : t -> Id.CompDest.t Name.Hamt.t

  val has_destructor_with_name : Name.t -> t -> bool
end

(** Computation-level type destructor declarations. *)
module CompDest : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make :
       id:Id.CompDest.t
    -> name:Name.t
    -> location:Location.t
    -> implicit_arguments:int
    -> mctx:LF.mctx
    -> observation_typ:Comp.typ
    -> return_typ:Comp.typ
    -> kind:Id.CompCotyp.t
    -> t

  (** {1 Destructors}*)

  val id : t -> Id.Const.t

  val location : t -> Location.t

  val name : t -> Name.t

  val implicit_arguments : t -> int

  val mctx : t -> LF.mctx

  val observation_typ : t -> Comp.typ

  val return_typ : t -> Comp.typ

  val kind : t -> Id.CompCotyp.t
end

(** Computation declarations. *)
module Comp : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make :
       id:Id.Comp.t
    -> name:string
    -> location:Location.t
    -> implicit_arguments:int
    -> typ:Comp.typ
    -> ?mutual_group:Id.Comp.t Nonempty.t Option.t
    -> Comp.value
    -> t

  (** {1 Destructors} *)

  val id : t -> Id.Comp.t

  val location : t -> Location.t

  val name : t -> Name.t

  val implicit_arguments : t -> int

  val typ : t -> Comp.typ

  val program : t -> Comp.value

  val mutual_group : t -> Id.Comp.t Nonempty.t Option.t
end

(** Specifications for sets of contexts. *)
module Schema : sig
  open Syntax.Int

  type t

  val make :
    id:Id.Schema.t -> name:Name.t -> location:Location.t -> LF.schema -> t

  val id : t -> Id.Schema.t

  val name : t -> Name.t

  val location : t -> Location.t

  val schema : t -> LF.schema
end

(** Namespace for declarations. *)
module Module : sig
  type ('signature, 'declaration, 'declaration_with_id) t

  (** {1 Constructors} *)

  val make_empty :
    id:Id.Module.t -> location:Location.t -> Name.t -> (_, _, _) t

  val add_declaration :
       ('signature, 'declaration, 'declaration_with_id) t
    -> 'signature * 'declaration
    -> ('signature, 'declaration, 'declaration_with_id) t

  val add_to_name_index :
       ('signature, 'declaration, 'declaration_with_id) t
    -> Name.t
    -> 'signature * 'declaration_with_id
    -> ('signature, 'declaration, 'declaration_with_id) t

  (** {1 Destructors} *)

  val id : (_, _, _) t -> Id.Comp.t

  val location : (_, _, _) t -> Location.t

  val name : (_, _, _) t -> Name.t

  val declarations :
    ('signature, 'declaration, _) t -> ('signature * 'declaration) List.t

  (** {1 Lookups} *)

  val lookup :
       ('signature, _, 'declaration_with_id) t
    -> Name.t
    -> ('signature * 'declaration_with_id) Option.t

  (** {1 Iterators} *)

  val fold_declarations :
       ('a -> 'signature * 'declaration -> 'a)
    -> 'a
    -> ('signature, 'declaration, _) t
    -> 'a
end

(** Documentation comments.

    These are declared as [%{{ content }}%] in the external syntax. *)
module DocumentationComment : sig
  type t

  (** {1 Constructors} *)

  val make : location:Location.t -> string -> t

  (** {1 Destructors} *)

  val content : t -> string

  val location : t -> Location.t
end

(** Logic programming query declarations on LF type. *)
module Query : sig
  open Syntax.Int

  type t

  type search_parameters =
    { expected_solutions : int Option.t
    ; maximum_tries : int Option.t
    ; search_depth : int Option.t
    }

  (** {1 Constructors} *)

  val make_search_parameters :
       ?expected_solutions:int Option.t
    -> ?maximum_tries:int Option.t
    -> ?search_depth:int Option.t
    -> unit
    -> search_parameters

  val make :
       id:Id.Query.t
    -> location:Location.t
    -> ?name:string Option.t
    -> ?search_parameters:search_parameters
    -> LF.mctx * (LF.typ * offset)
    -> t

  (** {1 Destructors} *)

  val id : t -> Id.Query.t

  val location : t -> Location.t

  val name : t -> Name.t Option.t

  val query : t -> LF.mctx * (LF.typ * offset)

  val search_parameters : t -> search_parameters
end

(** Logic programming query declarations on computational types. *)
module MQuery : sig
  open Syntax.Int

  type t

  type search_parameters =
    { expected_solutions : int Option.t
    ; search_tries : int Option.t
    ; search_depth : int Option.t
    ; split_index : int Option.t
    }

  (** {1 Constructors} *)

  val make_search_parameters :
       ?expected_solutions:int Option.t
    -> ?search_tries:int Option.t
    -> ?search_depth:int Option.t
    -> ?split_index:int Option.t
    -> unit
    -> search_parameters

  val make :
       id:Id.MQuery.t
    -> location:Location.t
    -> ?name:string Option.t
    -> ?search_parameters:search_parameters
    -> Syntax.Int.Comp.typ * offset
    -> t

  (** {1 Destructors} *)

  val id : t -> Id.Query.t

  val location : t -> Location.t

  val name : t -> Name.t Option.t

  val query : t -> Comp.typ * offset

  val search_parameters : t -> search_parameters
end

(** The type of Beluga signatures. *)
type t

type mutually_recursive_typs =
  [ `Typs of (Typ.t * Const.t Name.LinkedHamt.t) Nonempty.t ]

type mutually_recursive_comp_typs =
  [ `Comp_typs of
    [ `Comp_typ of CompTyp.t * CompConst.t Name.LinkedHamt.t
    | `Comp_cotyp of CompCotyp.t * CompDest.t Name.LinkedHamt.t
    ]
    Nonempty.t
  ]

type mutually_recursive_programs = [ `Programs of Comp.t Name.LinkedHamt1.t ]

(** The type of declarations in Beluga signatures. *)
type declaration =
  [ `Typ_declaration of Typ.t
  | `Const_declaration of Const.t
  | `Comp_typ_declaration of CompTyp.t
  | `Comp_const_declaration of CompConst.t
  | `Comp_cotyp_declaration of CompCotyp.t
  | `Comp_dest_declaration of CompDest.t
  | `Comp_declaration of Comp.t
  | `Schema_declaration of Schema.t
  | `Module_declaration of (t, declaration, declaration_with_id) Module.t
  | `Documentation_comment of DocumentationComment.t
  | `Mutually_recursive_declaration of
    [ mutually_recursive_typs
    | mutually_recursive_comp_typs
    | mutually_recursive_programs
    ]
  | `Query_declaration of Query.t
  | `MQuery_declaration of MQuery.t
  ]

(** The subtype of declarations having and ID associated with them. *)
and declaration_with_id =
  [ `Typ_declaration of Typ.t
  | `Const_declaration of Const.t
  | `Comp_typ_declaration of CompTyp.t
  | `Comp_const_declaration of CompConst.t
  | `Comp_cotyp_declaration of CompCotyp.t
  | `Comp_dest_declaration of CompDest.t
  | `Comp_declaration of Comp.t
  | `Schema_declaration of Schema.t
  | `Module_declaration of (t, declaration, declaration_with_id) Module.t
  | `Query_declaration of Query.t
  | `MQuery_declaration of MQuery.t
  ]

(** {1 Lookups by Qualified Name} *)

(** Lookups by qualified name allow for looking up the entry currently in
    scope at a given path. That is, if a lookup results in some declaration,
    then that declaration is in scope for the signature in which it was
    looked up.

    Looked up declarations are associated with the signature up to and
    including that declaration. This allows for subsequent lookups to be made
    on the signature as of that declaration being made. *)

(** [lookup signature name] returns [None] if there is no declaration in
    [signature] having name [name], and otherwise returns
    [Some (signature', declaration)] where [signature'] is the signature up
    to and including [declaration] and [declaration] is the latest
    declaration in [signature] having name [name]. *)
val lookup : t -> QualifiedName.t -> (t * declaration_with_id) Option.t

(** [lookup_typ signature qualified_name] is the latest LF type family
    declared at [qualified_name] in [signature] if such a declaration exists. *)
val lookup_typ : t -> QualifiedName.t -> (t * Typ.t) Option.t

(** [lookup_constructor signature qualified_name] is the latest LF type
    constructor declared at [qualified_name] in [signature] if such a
    declaration exists. *)
val lookup_constructor : t -> QualifiedName.t -> (t * Const.t) Option.t

(** [lookup_comp_typ signature qualified_name] is the latest computational
    type declared at [qualified_name] in [signature] if such a declaration
    exists. *)
val lookup_comp_typ : t -> QualifiedName.t -> (t * CompTyp.t) Option.t

(** [lookup_comp_constructor signature qualified_name] is the latest
    computational type constructor declared at [qualified_name] in
    [signature] if such a declaration exists. *)
val lookup_comp_constructor :
  t -> QualifiedName.t -> (t * CompConst.t) Option.t

(** [lookup_comp_cotyp signature qualified_name] is the latest computational
    cotype declared at [qualified_name] in [signature] if such a declaration
    exists. *)
val lookup_comp_cotyp : t -> QualifiedName.t -> (t * CompCotyp.t) Option.t

(** [lookup_comp_destructor signature qualified_name] is the latest
    computational cotype destructor declared at [qualified_name] in
    [signature] if such a declaration exists. *)
val lookup_comp_destructor :
  t -> QualifiedName.t -> (t * CompDest.t) Option.t

(** [lookup_comp signature qualified_name] is the latest computation declared
    at [qualified_name] in [signature] if such a declaration exists. *)
val lookup_comp : t -> QualifiedName.t -> (t * Comp.t) Option.t

(** [lookup_schema signature qualified_name] is the latest context schema
    declared at [qualified_name] in [signature] if such a declaration exists. *)
val lookup_schema : t -> QualifiedName.t -> (t * Schema.t) Option.t

(** [lookup_module signature qualified_name] is the latest module declared at
    [qualified_name] in [signature] if such a declaration exists. *)
val lookup_module :
     t
  -> QualifiedName.t
  -> (t * (t, declaration, declaration_with_id) Module.t) Option.t

(** [lookup_query signature qualified_name] is the latest logic programming
    query on LF types declared at [qualified_name] in [signature] if such a
    declaration exists. *)
val lookup_query : t -> QualifiedName.t -> (t * Query.t) Option.t

(** [lookup_mquery signature qualified_name] is the latest logic programming
    meta-query on computational types declared at [qualified_name] in
    [signature] if such a declaration exists. *)
val lookup_mquery : t -> QualifiedName.t -> (t * MQuery.t) Option.t

(** {1 Lookups by ID} *)

(** Lookups by ID in a signature allows for bypassing scope checks.
    Declaration IDs were allocated during signature reconstruction as
    declarations were added. Since IDs are floating references, then a
    declaration looked up by ID in a signature is its latest version. That
    is, if the declaration associated with a given ID was altered since it
    was first added, then the lookup by ID returns the altered version of the
    declaration. *)

(** [lookup_typ_by_id signature id] is the LF type family having ID [id] in
    [signature] if such a declaration exists. *)
val lookup_typ_by_id : t -> Id.Typ.t -> (t * Typ.t) Option.t

(** [lookup_constructor_by_id signature id] is the LF type constructor having
    ID [id] in [signature] if such a declaration exists. *)
val lookup_constructor_by_id : t -> Id.Const.t -> (t * Const.t) Option.t

(** [lookup_comp_typ_by_id signature id] is the computational type having ID
    [id] in [signature] if such a declaration exists. *)
val lookup_comp_typ_by_id : t -> Id.CompTyp.t -> (t * CompTyp.t) Option.t

(** [lookup_comp_constructor_by_id signature id] is the computational type
    constructor having ID [id] in [signature] if such a declaration exists. *)
val lookup_comp_constructor_by_id :
  t -> Id.CompConst.t -> (t * CompConst.t) Option.t

(** [lookup_comp_cotyp_by_id signature id] is the computational cotype having
    ID [id] in [signature] if such a declaration exists. *)
val lookup_comp_cotyp_by_id :
  t -> Id.CompCotyp.t -> (t * CompCotyp.t) Option.t

(** [lookup_comp_destructor_by_id signature id] is the computational cotype
    destructor having ID [id] in [signature] if such a declaration exists. *)
val lookup_comp_destructor_by_id :
  t -> Id.CompDest.t -> (t * CompDest.t) Option.t

(** [lookup_comp_by_id signature id] is the computation having ID [id] in
    [signature] if such a declaration exists. *)
val lookup_comp_by_id : t -> Id.Comp.t -> (t * Comp.t) Option.t

(** [lookup_schema_by_id signature id] is the schema having ID [id] in
    [signature] if such a declaration exists. *)
val lookup_schema_by_id : t -> Id.Schema.t -> (t * Schema.t) Option.t

(** [lookup_module_by_id signature id] is the module having ID [id] in
    [signature] if such a declaration exists. *)
val lookup_module_by_id :
     t
  -> Id.Module.t
  -> (t * (t, declaration, declaration_with_id) Module.t) Option.t

(** [lookup_query_by_id signature id] is the logic programming query having
    ID [id] in [signature] if such a declaration exists. *)
val lookup_query_by_id : t -> Id.Query.t -> (t * Query.t) Option.t

(** [lookup_query_by_id signature id] is the logic programming meta-query
    having ID [id] in [signature] if such a declaration exists. *)
val lookup_mquery_by_id : t -> Id.MQuery.t -> (t * MQuery.t) Option.t

(** {1 Unsafe lookups by ID} *)

(** Unsafe lookups by ID are functionally equivalent to safe lookups by ID,
    except that exceptions are raised when a lookup fails.

    These unsafe signature lookup functions are intended to be used when it
    is known that the given ID is bound in the signature and has the intended
    ID kind.

    The exception types they raise are strictly programmer errors, not user
    errors. *)

(** [UnboundId (id, signature)] is the exception raised when [id] could not
    be found in [signature]. *)
exception UnboundId of Id.t * t

type id_kind_mismatch =
  { bound : Id.t
  ; expected : Id.t
  ; signature : t
  }

(** [IdKindMismatch { bound; expected; signature }] is the exception raised
    when IDs [bound] and [expected] differ in the kind of ID looked up in
    [signature]. *)
exception IdKindMismatch of id_kind_mismatch

(** [lookup_typ_by_id_exn signature id] is the LF type family having ID [id]
    in [signature].

    @raise UnboundId If the ID [id] is not in [signature].
    @raise IdKindMismatch
      If the entry having the ID [id] in [signature] is not an LF type
      family. *)
val lookup_typ_by_id_exn : t -> Id.Typ.t -> t * Typ.t

(** [lookup_constructor_by_id_exn signature id] is the LF type constructor
    having ID [id] in [signature].

    @raise UnboundId If the ID [id] is not in [signature].
    @raise IdKindMismatch
      If the entry having the ID [id] in [signature] is not an LF type
      constructor. *)
val lookup_constructor_by_id_exn : t -> Id.Const.t -> t * Const.t

(** [lookup_comp_typ_by_id_exn signature id] is the computational type having
    ID [id] in [signature].

    @raise UnboundId If the ID [id] is not in [signature].
    @raise IdKindMismatch
      If the entry having the ID [id] in [signature] is not a computational
      type. *)
val lookup_comp_typ_by_id_exn : t -> Id.CompTyp.t -> t * CompTyp.t

(** [lookup_comp_constructor_by_id_exn signature id] is the computational
    type constructor having ID [id] in [signature].

    @raise UnboundId If the ID [id] is not in [signature].
    @raise IdKindMismatch
      If the entry having the ID [id] in [signature] is not a computational
      type constructor. *)
val lookup_comp_constructor_by_id_exn :
  t -> Id.CompConst.t -> t * CompConst.t

(** [lookup_comp_cotyp_by_id_exn signature id] is the computational cotype
    having ID [id] in [signature].

    @raise UnboundId If the ID [id] is not in [signature].
    @raise IdKindMismatch
      If the entry having the ID [id] in [signature] is not a computational
      cotype. *)
val lookup_comp_cotyp_by_id_exn : t -> Id.CompCotyp.t -> t * CompCotyp.t

(** [lookup_comp_destructor_by_id_exn signature id] is the computational
    cotype destructor having ID [id] in [signature].

    @raise UnboundId If the ID [id] is not in [signature].
    @raise IdKindMismatch
      If the entry having the ID [id] in [signature] is not a computational
      cotype destructor. *)
val lookup_comp_destructor_by_id_exn : t -> Id.CompDest.t -> t * CompDest.t

(** [lookup_comp_by_id_exn signature id] is the computation having ID [id] in
    [signature].

    @raise UnboundId If the ID [id] is not in [signature].
    @raise IdKindMismatch
      If the entry having the ID [id] in [signature] is not a computation. *)
val lookup_comp_by_id_exn : t -> Id.Comp.t -> t * Comp.t

(** [lookup_schema_by_id_exn signature id] is the schema having ID [id] in
    [signature].

    @raise UnboundId If the ID [id] is not in [signature].
    @raise IdKindMismatch
      If the entry having the ID [id] in [signature] is not a schema. *)
val lookup_schema_by_id_exn : t -> Id.Schema.t -> t * Schema.t

(** [lookup_module_by_id_exn signature id] is the module having ID [id] in
    [signature].

    @raise UnboundId If the ID [id] is not in [signature].
    @raise IdKindMismatch
      If the entry having the ID [id] in [signature] is not a module. *)
val lookup_module_by_id_exn :
  t -> Id.Query.t -> t * (t, declaration, declaration_with_id) Module.t

(** [lookup_query_by_id_exn signature id] is the logic programming query
    having ID [id] in [signature].

    @raise UnboundId If the ID [id] is not in [signature].
    @raise IdKindMismatch
      If the entry having the ID [id] in [signature] is not a logic
      programming query. *)
val lookup_query_by_id_exn : t -> Id.Query.t -> t * Query.t

(** [lookup_query_by_id_exn signature id] is the logic programming meta-query
    having ID [id] in [signature].

    @raise UnboundId If the ID [id] is not in [signature].
    @raise IdKindMismatch
      If the entry having the ID [id] in [signature] is not a logic
      programming meta-query. *)
val lookup_mquery_by_id_exn : t -> Id.MQuery.t -> t * MQuery.t

(** {1 Declarations} *)

(** [id_of_declaration declaration] is [Some id] with [id] being the lifted
    ID of [declaration] if one is found, and [None] otherwise. *)
val id_of_declaration : [< declaration ] -> Id.t Option.t

(** Exception raised when an ID cannot be found for a declaration. *)
exception DeclarationWithoutId of declaration

(** [id_of_declaration_exn declaration] is the lifted ID of [declaration].

    @raise DeclarationWithoutId
      If there is no ID associated with [declaration]. *)
val id_of_declaration_exn : [< declaration ] -> Id.t

(** {1 Paths} *)

(** Qualified names define paths to entries in a signature.

    In a signature, a path to an entry is valid if it is not shadowed by a
    later and equal path to a different entry. *)

(** [is_path_to_entry signature id path] is [Some (signature', declaration)]
    if looking up the entry in [signature] by ID [id] or qualified name
    [path] result in the same declaration [declaration], with [signature']
    being the signature up to and including [declaration]. If the looked up
    declarations differ or do not exist, then [None] is returned. *)
val is_path_to_entry :
  t -> Id.t -> QualifiedName.t -> (t * declaration_with_id) Option.t

(** [all_paths_to_entry signature id] is the set of all qualified names in
    scope that may be used to refer to the declaration having ID [id] in
    [signature]. *)
val all_paths_to_entry :
  t -> Id.t -> (QualifiedName.Set.t, [> `Unbound_id of Id.t * t ]) Result.t

(** [all_paths_to_entry_exn signature id] is
    [all_paths_to_entry signature id].

    @raise UnboundId If the ID [id] is not in [signature]. *)
val all_paths_to_entry_exn : t -> Id.t -> QualifiedName.Set.t

