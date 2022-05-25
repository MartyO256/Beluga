(** Beluga signatures.

    A Beluga signature is an append-only database for declarations. It is an
    immutable data structure, meaning that a mutation to a signature creates
    a new signature. Significant data sharing is in place to ensure good
    performance.

    {2 Scope-Safe Lookups}

    Lookups in a Beluga signature respect scoping information if the
    signature is kept local to the function making the lookup. To jump at an
    earlier point in a signature, an added declaration is stored along with
    the signature up to and including that added declaration.

    {2 Freezable Declarations}

    Grouped declarations are defined in two stages: unfrozen and frozen.

    - An unfrozen declaration may have additions made to it in derived
      signatures.
    - A frozen declaration may not have such additions.

    Grouped declarations include LF type constants {!Typ},
    computational-level data type constants {!CompTyp}, computational-level
    codata type constants {!CompCotyp}, and namespaces {!Module}.

    Freezable declarations are frozen whenever:

    - the freezable declaration goes out of scope,
    - a module declaration is added,
    - a logic programming query is added, or
    - a theorem is added.

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
  (** {1 ID Kinds} *)

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

(** LF type family declarations. *)
module Typ : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make_initial_declaration :
       id:Id.Typ.t
    -> name:Name.t
    -> location:Location.t
    -> ?documentation_comment:DocumentationComment.t
    -> LF.kind
    -> t

  (** {1 Destructors} *)

  val id : t -> Id.Typ.t

  val location : t -> Location.t

  val name : t -> Name.t

  val kind : t -> LF.kind

  val arguments : t -> int

  val implicit_arguments : t -> int

  val explicit_arguments : t -> int

  val documentation_comment : t -> DocumentationComment.t Option.t

  (** {1 Freezing} *)

  exception UnfrozenTyp of t

  exception FrozenTyp of t

  val is_frozen : t -> bool

  val is_unfrozen : t -> bool

  (** [freeze ~term_subordinates ~type_subordinated_to tA] is the frozen
      version of [tA] using the input subordination information.

      @raise FrozenTyp If [tA] is frozen. *)
  val freeze :
       term_subordinates:Id.Typ.Set.t
    -> type_subordinated_to:Id.Typ.Set.t
    -> t
    -> t

  (** {1 LF Constructors} *)

  exception TypNameCollision of Name.t * Id.Const.t * t

  exception ConstNameCollision of Name.t * Id.Const.t * t

  (** [add_constructor name tM tA] adds the constructor [tM] having name
      [name] to the type family [tA].

      @raise FrozenTyp If [tA] is frozen.
      @raise TypNameCollision If [tA] and [tM] have the same name.
      @raise ConstNameCollision
        If there exists a constructor in [tA] having the same name as [tM]. *)
  val add_constructor : Name.t -> Id.Const.t -> t -> t

  val constructors : t -> Id.Const.t Name.Hamt.t

  val has_constructor_with_name : Name.t -> t -> bool

  (** {1 Naming} *)

  val fresh_var_name :
    t -> ?default_base_name:string -> Name.fresh_name_supplier

  val fresh_mvar_name :
    t -> ?default_base_name:string -> Name.fresh_name_supplier

  val set_var_naming_convention : string Option.t -> t -> t

  val set_mvar_naming_convention : string Option.t -> t -> t

  val set_naming_conventions :
    var:string Option.t -> mvar:string Option.t -> t -> t

  (** {1 Subordination} *)

  (** [is_term_subordinate tA tB_id] is [true] if and only if the LF family
      having ID [tB_id] is term-level subordinate to [tA].

      This is determined using the subordination data passed when [tA] was
      frozen with {!freeze}.

      @raise FrozenTyp If [tA] is frozen. *)
  val is_term_subordinate : t -> Id.Typ.t -> bool

  (** [is_type_subordinate_to tA tB_id] is [true] if and only if the LF
      family [tA] is type-level subordinate to the LF family having ID
      [tB_id].

      If [tA] is type-level subordinate to [tB], then [tA]-terms can contain
      [tB]-terms. Type-level subordination is not transitive.

      For instance, given the following frozen signature:

      {v
        nat : type.
        list : nat -> type.
        t : list (suc (suc z)) -> type.
        t' : list (suc (suc N)) -> type.
      v}

      We have that:

      - [is_type_subordinate_to list (id nat) = true]
      - [is_type_subordinate_to t (id list) = true]
      - [is_type_subordinate_to t' (id list) = true]
      - [is_type_subordinate_to t' (id nat) = true]

      And:

      - [is_type_subordinate_to nat (id nat) = false]
      - [is_type_subordinate_to nat (id list) = false]
      - [is_type_subordinate_to nat (id t) = false]
      - [is_type_subordinate_to nat (id t') = false]
      - [is_type_subordinate_to list (id t) = false]
      - [is_type_subordinate_to list (id t') = false]
      - [is_type_subordinate_to t (id nat) = false]
      - [is_type_subordinate_to t (id t') = false]
      - [is_type_subordinate_to t' (id t') = false]

      Type-level subordination is determined using the subordination data
      passed when [tA] was frozen with {!freeze}.

      @raise FrozenTyp If [tA] is frozen. *)
  val is_type_subordinate_to : t -> Id.Typ.t -> bool
end

(** LF type constructor declarations. *)
module Const : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make :
       id:Id.Const.t
    -> name:Name.t
    -> location:Location.t
    -> implicit_arguments:int
    -> kind:Id.Typ.t
    -> ?documentation_comment:DocumentationComment.t
    -> LF.typ
    -> t

  (** {1 Destructors}*)

  val id : t -> Id.Const.t

  val location : t -> Location.t

  val name : t -> Name.t

  val typ : t -> LF.typ

  val kind : t -> Id.Typ.t

  val documentation_comment : t -> DocumentationComment.t Option.t
end

(** Computation-level data type constant declarations. *)
module CompTyp : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make_initial_declaration :
       id:Id.CompTyp.t
    -> name:Name.t
    -> location:Location.t
    -> implicit_arguments:int
    -> positivity:Sgn.positivity_flag
    -> ?documentation_comment:DocumentationComment.t
    -> Comp.kind
    -> t

  (** {1 Destructors} *)

  val id : t -> Id.CompTyp.t

  val location : t -> Location.t

  val name : t -> Name.t

  val kind : t -> Comp.kind

  val documentation_comment : t -> DocumentationComment.t Option.t

  (** {1 Freezing} *)

  exception UnfrozenCompTyp of t

  exception FrozenCompTyp of t

  val is_frozen : t -> bool

  val is_unfrozen : t -> bool

  (** [freeze cK] is the frozen version of [cK].

      @raise FrozenCompTyp If [cK] is frozen. *)
  val freeze : t -> t

  (** {1 Constructors} *)

  exception CompTypNameCollision of Name.t * Id.CompConst.t * t

  exception CompConstNameCollision of Name.t * Id.CompConst.t * t

  (** [add_constructor name cM cK] adds the constructor [cM] having name
      [name] to the computational type family [cK].

      @raise FrozenCompTyp If [cK] is frozen.
      @raise CompTypNameCollision If [cK] and [cM] have the same name.
      @raise CompConstNameCollision
        If there exists a constructor in [cK] having the same name as [cM]. *)
  val add_constructor : Name.t -> Id.CompConst.t -> t -> t

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
    -> ?documentation_comment:DocumentationComment.t
    -> Comp.typ
    -> t

  (** {1 Destructors}*)

  val id : t -> Id.CompConst.t

  val location : t -> Location.t

  val name : t -> Name.t

  val implicit_arguments : t -> int

  val typ : t -> Comp.typ

  val kind : t -> Id.CompTyp.t

  val documentation_comment : t -> DocumentationComment.t Option.t
end

(** Computation-level codata type constant declarations. *)
module CompCotyp : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make_initial_declaration :
       id:Id.CompCotyp.t
    -> name:Name.t
    -> location:Location.t
    -> implicit_arguments:int
    -> ?documentation_comment:DocumentationComment.t
    -> Comp.kind
    -> t

  (** {1 Destructors} *)

  val id : t -> Id.CompCotyp.t

  val location : t -> Location.t

  val name : t -> Name.t

  val kind : t -> Comp.kind

  val documentation_comment : t -> DocumentationComment.t Option.t

  (** {1 Freezing} *)

  exception UnfrozenCompCotyp of t

  exception FrozenCompCotyp of t

  val is_frozen : t -> bool

  val is_unfrozen : t -> bool

  (** [freeze cK] is the frozen version of [cK].

      @raise FrozenCompCotyp If [cK] is frozen. *)
  val freeze : t -> t

  (** {1 Destructors} *)

  exception CompCotypNameCollision of Name.t * Id.CompDest.t * t

  exception CompDestNameCollision of Name.t * Id.CompDest.t * t

  (** [add_destructor name cM cK] adds the destructor [cM] having name [name]
      to the computational type family [cK].

      @raise FrozenCompCotyp If [cK] is frozen.
      @raise CompCotypNameCollision If [cK] and [cM] have the same name.
      @raise CompDestNameCollision
        If there exists a constructor in [cK] having the same name as [cM]. *)
  val add_destructor : Name.t -> Id.CompDest.t -> t -> t

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
    -> ?documentation_comment:DocumentationComment.t
    -> Id.CompCotyp.t
    -> t

  (** {1 Destructors}*)

  val id : t -> Id.CompDest.t

  val location : t -> Location.t

  val name : t -> Name.t

  val implicit_arguments : t -> int

  val mctx : t -> LF.mctx

  val observation_typ : t -> Comp.typ

  val return_typ : t -> Comp.typ

  val kind : t -> Id.CompCotyp.t

  val documentation_comment : t -> DocumentationComment.t Option.t
end

(** Computation declarations. *)
module Comp : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make :
       id:Id.Comp.t
    -> name:Name.t
    -> location:Location.t
    -> implicit_arguments:int
    -> typ:Comp.typ
    -> ?mutual_group:Id.Comp.t List1.t
    -> ?documentation_comment:DocumentationComment.t
    -> Comp.value
    -> t

  (** {1 Destructors} *)

  val id : t -> Id.Comp.t

  val location : t -> Location.t

  val name : t -> Name.t

  val implicit_arguments : t -> int

  val typ : t -> Comp.typ

  val program : t -> Comp.value

  val mutual_group : t -> Id.Comp.t List1.t Option.t

  val documentation_comment : t -> DocumentationComment.t Option.t
end

(** Specifications for sets of contexts. *)
module Schema : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make :
       id:Id.Schema.t
    -> name:Name.t
    -> location:Location.t
    -> ?documentation_comment:DocumentationComment.t
    -> LF.schema
    -> t

  (** {1 Destructors} *)

  val id : t -> Id.Schema.t

  val name : t -> Name.t

  val location : t -> Location.t

  val schema : t -> LF.schema

  val documentation_comment : t -> DocumentationComment.t Option.t
end

(** Namespace for declarations. *)
module Module : sig
  type ('signature, 'entry, 'declaration) t

  (** {1 Constructors} *)

  val make_empty :
       id:Id.Module.t
    -> location:Location.t
    -> ?documentation_comment:DocumentationComment.t
    -> Name.t
    -> (_, _, _) t

  val add_entry :
       ('signature, 'entry, 'declaration) t
    -> 'signature * 'entry
    -> ('signature, 'entry, 'declaration) t

  val add_binding :
       ('signature, 'entry, 'declaration) t
    -> Name.t
    -> 'signature * 'declaration
    -> ('signature, 'entry, 'declaration) t

  (** {1 Destructors} *)

  val id : (_, _, _) t -> Id.Module.t

  val location : (_, _, _) t -> Location.t

  val name : (_, _, _) t -> Name.t

  val entries : ('signature, 'entry, _) t -> ('signature * 'entry) List.t

  val documentation_comment : (_, _, _) t -> DocumentationComment.t Option.t

  (** {1 Lookups} *)

  val lookup :
       ('signature, _, 'declaration) t
    -> Name.t
    -> ('signature * 'declaration) Option.t

  (** {1 Iterators} *)

  val fold_entries :
       ('a -> 'signature * 'entry -> 'a)
    -> 'a
    -> ('signature, 'entry, _) t
    -> 'a
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
       ?expected_solutions:int
    -> ?maximum_tries:int
    -> ?search_depth:int
    -> unit
    -> search_parameters

  val make :
       id:Id.Query.t
    -> location:Location.t
    -> ?name:Name.t
    -> ?search_parameters:search_parameters
    -> ?documentation_comment:DocumentationComment.t
    -> LF.mctx * (LF.typ * offset)
    -> t

  (** {1 Destructors} *)

  val id : t -> Id.Query.t

  val location : t -> Location.t

  val name : t -> Name.t Option.t

  val query : t -> LF.mctx * (LF.typ * offset)

  val search_parameters : t -> search_parameters

  val documentation_comment : t -> DocumentationComment.t Option.t
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
       ?expected_solutions:int
    -> ?search_tries:int
    -> ?search_depth:int
    -> ?split_index:int
    -> unit
    -> search_parameters

  val make :
       id:Id.MQuery.t
    -> location:Location.t
    -> ?name:Name.t
    -> ?search_parameters:search_parameters
    -> ?documentation_comment:DocumentationComment.t
    -> Syntax.Int.Comp.typ * offset
    -> t

  (** {1 Destructors} *)

  val id : t -> Id.MQuery.t

  val location : t -> Location.t

  val name : t -> Name.t Option.t

  val query : t -> Comp.typ * offset

  val search_parameters : t -> search_parameters

  val documentation_comment : t -> DocumentationComment.t Option.t
end

(** Pragma for modifying the naming conventions of an LF type constant. *)
module NamePragma : sig
  type t

  (** {1 Constructor} *)

  val make :
       location:Location.t
    -> var_naming_convention:string Option.t
    -> mvar_naming_convention:string
    -> typ:Id.Typ.t
    -> t

  (** {1 Destructors} *)

  val location : t -> Location.t

  val var_naming_convention : t -> string Option.t

  val mvar_naming_convention : t -> string

  val typ : t -> Id.Typ.t
end

(** The type of Beluga signatures. *)
type t

(** The type of entries in Beluga signatures. *)
type entry

(** The subtype of entries having an ID associated with them.

    Declarations may introduce names in the scope or appear in relationships
    with other declarations. *)
type declaration

(** {1 Exceptions} *)

exception UnboundDeclaration of QualifiedName.t * t

exception BoundTypId of Id.Typ.t * t

exception UnboundTypId of Id.Typ.t * t

exception UnboundTyp of QualifiedName.t * t

exception BoundConstId of Id.Const.t * t

exception UnboundConstId of Id.Const.t * t

exception UnboundConst of QualifiedName.t * t

exception BoundCompTypId of Id.CompTyp.t * t

exception UnboundCompTypId of Id.CompTyp.t * t

exception UnboundCompTyp of QualifiedName.t * t

exception BoundCompConstId of Id.CompConst.t * t

exception UnboundCompConstId of Id.CompConst.t * t

exception UnboundCompConst of QualifiedName.t * t

exception BoundCompCotypId of Id.CompCotyp.t * t

exception UnboundCompCotypId of Id.CompCotyp.t * t

exception UnboundCompCotyp of QualifiedName.t * t

exception BoundCompDestId of Id.CompDest.t * t

exception UnboundCompDestId of Id.CompDest.t * t

exception UnboundCompDest of QualifiedName.t * t

exception BoundCompId of Id.Comp.t * t

exception UnboundCompId of Id.Comp.t * t

exception UnboundComp of QualifiedName.t * t

exception BoundSchemaId of Id.Schema.t * t

exception UnboundSchemaId of Id.Schema.t * t

exception UnboundSchema of QualifiedName.t * t

exception BoundModuleId of Id.Module.t * t

exception UnboundModuleId of Id.Module.t * t

exception UnboundModule of QualifiedName.t * t

exception BoundQueryId of Id.Query.t * t

exception UnboundQueryId of Id.Query.t * t

exception UnboundQuery of QualifiedName.t * t

exception BoundMQueryId of Id.MQuery.t * t

exception UnboundMQueryId of Id.MQuery.t * t

exception UnboundMQuery of QualifiedName.t * t

(** {1 Constructors} *)

(** The empty Beluga signature. *)
val empty : t

(** [add_typ signature tA] constructs the signature derived from [signature]
    with the addition of the LF type constant [tA] as a top-level
    declaration.

    In the resultant signature, the declaration having the same name as [tA]
    in [signature] is shadowed if there is any, and frozen if applicable, and
    [tA] is added as unfrozen if it is unfrozen.

    @raise BoundTypId *)
val add_typ : t -> Typ.t -> t

(** [add_const signature tM] constructs the signature derived from
    [signature] with the addition of the LF term constant [tM] as a top-level
    declaration.

    In the resultant signature, the declaration having the same name as [tM]
    in [signature] is shadowed if there is any, and frozen if applicable, and
    the target LF type-level constant for [tM] is updated to have [tM] as
    additional constructor.

    @raise BoundConstId
    @raise FrozenTyp
    @raise TypNameCollision
    @raise ConstNameCollision *)
val add_const : t -> Const.t -> t

(** [add_comp_typ signature cA] constructs the signature derived from
    [signature] with the addition of the computational type constant [cA] as
    a top-level declaration.

    In the resultant signature, the declaration having the same name as [cA]
    in [signature] is shadowed if there is any, and frozen if applicable, and
    [cA] is added as unfrozen if it is unfrozen.

    @raise BoundCompConstId *)
val add_comp_typ : t -> CompTyp.t -> t

(** [add_comp_const signature cM] constructs the signature derived from
    [signature] with the addition of the computational constructor constant
    [cM] as a top-level declaration.

    In the resultant signature, the declaration having the same name as [cM]
    in [signature] is shadowed if there is any, and frozen if applicable, and
    the target computational type constant for [cM] is updated to have [cM]
    as additional constructor.

    @raise BoundCompConstId
    @raise FrozenCompTyp
    @raise CompTypNameCollision
    @raise CompConstNameCollision *)
val add_comp_const : t -> CompConst.t -> t

(** [add_comp_cotyp signature cA] constructs the signature derived from
    [signature] with the addition of the computational cotype constant [cA]
    as a top-level declaration.

    In the resultant signature, the declaration having the same name as [cA]
    in [signature] is shadowed if there is any, and frozen if applicable, and
    [cA] is added as unfrozen if it is unfrozen.

    @raise BoundCompCotypId *)
val add_comp_cotyp : t -> CompCotyp.t -> t

(** [add_comp_dest signature cM] constructs the signature derived from
    [signature] with the addition of the computational destructor constant
    [cM] as a top-level declaration.

    In the resultant signature, the declaration having the same name as [cM]
    in [signature] is shadowed if there is any, and frozen if applicable, and
    the target computational cotype constant for [cM] is updated to have [cM]
    as additional destructor.

    @raise BoundCompDestId
    @raise FrozenCompCotyp
    @raise CompCotypNameCollision
    @raise CompDestNameCollision *)
val add_comp_dest : t -> CompDest.t -> t

(** [add_query signature query] constructs the signature derived from
    [signature] with the addition of the logic programming query on [query]
    as a top-level declaration.

    In the resultant signature, all declarations are frozen.

    @raise BoundQueryId *)
val add_query : t -> Query.t -> t

(** [add_mquery signature mquery] constructs the signature derived from
    [signature] with the addition of the logic programming query on
    computational types [mquery] as a top-level declaration.

    In the resultant signature, all declarations are frozen.

    @raise BoundMQueryId *)
val add_mquery : t -> MQuery.t -> t

(** [add_name_pragma signature name_pragma] constructs the signature derived
    from [signature] with the addition of the name pragma [name_pragma] for
    setting new variable naming conventions for witnesses to an LF type
    family.

    @raise UnboundTypId *)
val add_name_pragma : t -> NamePragma.t -> t

(** [add_documentation_comment signature comment] constructs the signature
    derived from [signature] with the addition of the documentation comment
    [comment]. *)
val add_documentation_comment : t -> DocumentationComment.t -> t

(** {1 Lookups} *)

(** Lookups by qualified name allow for looking up the declaration currently
    in scope at a given path. That is, if a lookup results in some
    declaration, then that declaration is in scope for the signature in which
    it was looked up.

    Looked up declarations are associated with the signature up to and
    including that declaration. This allows for subsequent lookups to be made
    on the signature as of that declaration being made.

    Lookups by ID in a signature allow for bypassing scope checks.
    Declaration IDs were allocated during signature reconstruction as
    declarations were added. Since IDs are floating references, then a
    declaration looked up by ID in a signature is its latest version. That
    is, if the declaration associated with a given ID was altered since it
    was first added, then the lookup by ID returns the altered version of the
    declaration.

    Unsafe lookups by ID are functionally equivalent to safe lookups by ID,
    except that exceptions are raised when a lookup fails. These unsafe
    signature lookup functions are intended to be used when it is known that
    the given ID is bound in the signature and has the intended ID kind. The
    exception types they raise are strictly programmer errors, not user
    errors. *)

val lookup : t -> QualifiedName.t -> declaration

val lookup' : t -> QualifiedName.t -> t * declaration

val lookup_opt : t -> QualifiedName.t -> declaration Option.t

val lookup_opt' : t -> QualifiedName.t -> (t * declaration) Option.t

(** {2 Typ} *)

val lookup_typ : t -> QualifiedName.t -> Typ.t

val lookup_typ' : t -> QualifiedName.t -> t * Typ.t

val lookup_typ_opt : t -> QualifiedName.t -> Typ.t Option.t

val lookup_typ_opt' : t -> QualifiedName.t -> (t * Typ.t) Option.t

val lookup_typ_by_id : t -> Id.Typ.t -> Typ.t

val lookup_typ_by_id' : t -> Id.Typ.t -> t * Typ.t

val lookup_typ_by_id_opt : t -> Id.Typ.t -> Typ.t Option.t

val lookup_typ_by_id_opt' : t -> Id.Typ.t -> (t * Typ.t) Option.t

(** {2 Const} *)

val lookup_const : t -> QualifiedName.t -> Const.t

val lookup_const' : t -> QualifiedName.t -> t * Const.t

val lookup_const_opt : t -> QualifiedName.t -> Const.t Option.t

val lookup_const_opt' : t -> QualifiedName.t -> (t * Const.t) Option.t

val lookup_const_by_id : t -> Id.Const.t -> Const.t

val lookup_const_by_id' : t -> Id.Const.t -> t * Const.t

val lookup_const_by_id_opt : t -> Id.Const.t -> Const.t Option.t

val lookup_const_by_id_opt' : t -> Id.Const.t -> (t * Const.t) Option.t

(** {2 Comp Typ} *)

val lookup_comp_typ : t -> QualifiedName.t -> CompTyp.t

val lookup_comp_typ' : t -> QualifiedName.t -> t * CompTyp.t

val lookup_comp_typ_opt : t -> QualifiedName.t -> CompTyp.t Option.t

val lookup_comp_typ_opt' : t -> QualifiedName.t -> (t * CompTyp.t) Option.t

val lookup_comp_typ_by_id : t -> Id.CompTyp.t -> CompTyp.t

val lookup_comp_typ_by_id' : t -> Id.CompTyp.t -> t * CompTyp.t

val lookup_comp_typ_by_id_opt : t -> Id.CompTyp.t -> CompTyp.t Option.t

val lookup_comp_typ_by_id_opt' :
  t -> Id.CompTyp.t -> (t * CompTyp.t) Option.t

(** {2 Comp Const} *)

val lookup_comp_const : t -> QualifiedName.t -> CompConst.t

val lookup_comp_const' : t -> QualifiedName.t -> t * CompConst.t

val lookup_comp_const_opt : t -> QualifiedName.t -> CompConst.t Option.t

val lookup_comp_const_opt' :
  t -> QualifiedName.t -> (t * CompConst.t) Option.t

val lookup_comp_const_by_id : t -> Id.CompConst.t -> CompConst.t

val lookup_comp_const_by_id' : t -> Id.CompConst.t -> t * CompConst.t

val lookup_comp_const_by_id_opt : t -> Id.CompConst.t -> CompConst.t Option.t

val lookup_comp_const_by_id_opt' :
  t -> Id.CompConst.t -> (t * CompConst.t) Option.t

(** {2 Comp Cotyp} *)

val lookup_comp_cotyp : t -> QualifiedName.t -> CompCotyp.t

val lookup_comp_cotyp' : t -> QualifiedName.t -> t * CompCotyp.t

val lookup_comp_cotyp_opt : t -> QualifiedName.t -> CompCotyp.t Option.t

val lookup_comp_cotyp_opt' :
  t -> QualifiedName.t -> (t * CompCotyp.t) Option.t

val lookup_comp_cotyp_by_id : t -> Id.CompCotyp.t -> CompCotyp.t

val lookup_comp_cotyp_by_id' : t -> Id.CompCotyp.t -> t * CompCotyp.t

val lookup_comp_cotyp_by_id_opt : t -> Id.CompCotyp.t -> CompCotyp.t Option.t

val lookup_comp_cotyp_by_id_opt' :
  t -> Id.CompCotyp.t -> (t * CompCotyp.t) Option.t

(** {2 Comp Dest} *)

val lookup_comp_dest : t -> QualifiedName.t -> CompDest.t

val lookup_comp_dest' : t -> QualifiedName.t -> t * CompDest.t

val lookup_comp_dest_opt : t -> QualifiedName.t -> CompDest.t Option.t

val lookup_comp_dest_opt' : t -> QualifiedName.t -> (t * CompDest.t) Option.t

val lookup_comp_dest_by_id : t -> Id.CompDest.t -> CompDest.t

val lookup_comp_dest_by_id' : t -> Id.CompDest.t -> t * CompDest.t

val lookup_comp_dest_by_id_opt : t -> Id.CompDest.t -> CompDest.t Option.t

val lookup_comp_dest_by_id_opt' :
  t -> Id.CompDest.t -> (t * CompDest.t) Option.t

(** {2 Comp} *)

val lookup_comp : t -> QualifiedName.t -> Comp.t

val lookup_comp' : t -> QualifiedName.t -> t * Comp.t

val lookup_comp_opt : t -> QualifiedName.t -> Comp.t Option.t

val lookup_comp_opt' : t -> QualifiedName.t -> (t * Comp.t) Option.t

val lookup_comp_by_id : t -> Id.Comp.t -> Comp.t

val lookup_comp_by_id' : t -> Id.Comp.t -> t * Comp.t

val lookup_comp_by_id_opt : t -> Id.Comp.t -> Comp.t Option.t

val lookup_comp_by_id_opt' : t -> Id.Comp.t -> (t * Comp.t) Option.t

(** {2 Schema} *)

val lookup_schema : t -> QualifiedName.t -> Schema.t

val lookup_schema' : t -> QualifiedName.t -> t * Schema.t

val lookup_schema_opt : t -> QualifiedName.t -> Schema.t Option.t

val lookup_schema_opt' : t -> QualifiedName.t -> (t * Schema.t) Option.t

val lookup_schema_by_id : t -> Id.Schema.t -> Schema.t

val lookup_schema_by_id' : t -> Id.Schema.t -> t * Schema.t

val lookup_schema_by_id_opt : t -> Id.Schema.t -> Schema.t Option.t

val lookup_schema_by_id_opt' : t -> Id.Schema.t -> (t * Schema.t) Option.t

(** {2 Module} *)

val lookup_module : t -> QualifiedName.t -> (t, entry, declaration) Module.t

val lookup_module' :
  t -> QualifiedName.t -> t * (t, entry, declaration) Module.t

val lookup_module_opt :
  t -> QualifiedName.t -> (t, entry, declaration) Module.t Option.t

val lookup_module_opt' :
  t -> QualifiedName.t -> (t * (t, entry, declaration) Module.t) Option.t

val lookup_module_by_id :
  t -> Id.Module.t -> (t, entry, declaration) Module.t

val lookup_module_by_id' :
  t -> Id.Module.t -> t * (t, entry, declaration) Module.t

val lookup_module_by_id_opt :
  t -> Id.Module.t -> (t, entry, declaration) Module.t Option.t

val lookup_module_by_id_opt' :
  t -> Id.Module.t -> (t * (t, entry, declaration) Module.t) Option.t

(** {2 Query} *)

val lookup_query : t -> QualifiedName.t -> Query.t

val lookup_query' : t -> QualifiedName.t -> t * Query.t

val lookup_query_opt : t -> QualifiedName.t -> Query.t Option.t

val lookup_query_opt' : t -> QualifiedName.t -> (t * Query.t) Option.t

val lookup_query_by_id : t -> Id.Query.t -> Query.t

val lookup_query_by_id' : t -> Id.Query.t -> t * Query.t

val lookup_query_by_id_opt : t -> Id.Query.t -> Query.t Option.t

val lookup_query_by_id_opt' : t -> Id.Query.t -> (t * Query.t) Option.t

(** {2 MQuery} *)

val lookup_mquery : t -> QualifiedName.t -> MQuery.t

val lookup_mquery' : t -> QualifiedName.t -> t * MQuery.t

val lookup_mquery_opt : t -> QualifiedName.t -> MQuery.t Option.t

val lookup_mquery_opt' : t -> QualifiedName.t -> (t * MQuery.t) Option.t

val lookup_mquery_by_id : t -> Id.MQuery.t -> MQuery.t

val lookup_mquery_by_id' : t -> Id.MQuery.t -> t * MQuery.t

val lookup_mquery_by_id_opt : t -> Id.MQuery.t -> MQuery.t Option.t

val lookup_mquery_by_id_opt' : t -> Id.MQuery.t -> (t * MQuery.t) Option.t

(** {1 Scanning} *)

val find_all_queries : t -> (t * Query.t) List.t

val find_all_mqueries : t -> (t * MQuery.t) List.t
