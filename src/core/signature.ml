(** Beluga signatures.

    @author Marc-Antoine Ouimet *)

open Support

module Name = struct
  type t = string

  include (Ord.Make (String) : Ord.ORD with type t := t)

  include (
    struct
      let pp = Format.pp_print_string

      let show = Fun.id
    end :
      Show.SHOW with type t := t)

  module Set = Set.Make (String)
  module Map = Map.Make (String)
  module LinkedMap = LinkedMap.Make (Map)
  module Hamt = Hamt.Make (String)
  module LinkedHamt = Support.LinkedHamt.Make (Hamt)
  module LinkedHamt1 = Support.LinkedHamt.Make1 (Hamt)

  type fresh_name_supplier = Set.t -> t

  (** [find_distinct names used_names] is the first name in [names] that is
      not a member of [used_names]. The elements in [names] are assumed to be
      all distinct.

      @raise Invalid_argument
        if the sequence [names] is fully exhausted without being able to
        generate a fresh name. *)
  let rec find_distinct : t Seq.t -> Set.t -> t =
   fun generate_name used_names ->
    match generate_name () with
    | Seq.Nil ->
      raise @@ Invalid_argument "Exhausted sequence of fresh names"
    | Seq.Cons (hd, tl) ->
      if Set.mem hd used_names then find_distinct tl used_names else hd

  (** [names_seq base index] is the sequence of names with prefix [base] and
      incremental integer suffix starting with [index].

      For instance, [names_seq "x" 1] is the sequence of names
      [\["x1"; "x2"; ...; "xM"\]] where ["M"] is [Int.max_int]. *)
  let rec names_seq : string -> int -> t Seq.t =
   fun base i ->
    Seq.cons
      (base ^ Int.show i)
      (if Int.(i = max_int) then Seq.empty
      else fun () -> names_seq base (i + 1) ())

  let prefixed_fresh_name_supplier base =
    find_distinct @@ Seq.cons base (names_seq base 1)
end

module QualifiedName = struct
  type t =
    { name : Name.t
    ; modules : Name.t List.t
    }

  let make ?(modules = []) name = { name; modules }

  let[@inline] name { name; _ } = name

  let[@inline] modules { modules; _ } = modules

  include (
    Show.Make (struct
      type nonrec t = t

      let pp ppf n =
        Format.fprintf ppf "%a::%a"
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf "::")
             (fun ppf x -> Format.fprintf ppf "%a" Name.pp x))
          (modules n) Name.pp (name n)
    end) :
      Show.SHOW with type t := t)

  include (
    Eq.Make (struct
      type nonrec t = t

      let equal x y =
        if Name.equal (name x) (name y) then
          List.equal Name.equal (modules x) (modules y)
        else false
    end) :
      Eq.EQ with type t := t)

  module Ord : Ord.ORD with type t = t = Ord.Make (struct
    type nonrec t = t

    module ModuleListOrd : Ord.ORD with type t = Name.t list =
      List.MakeOrd (Name)

    let compare x y =
      let comparison = ModuleListOrd.compare (modules x) (modules y) in
      if Stdlib.(comparison <> 0) then comparison
      else Name.compare (name x) (name y)
  end)

  include (Ord : Support.Ord.ORD with type t := t)

  module Set = Set.Make (Ord)
  module Map = Map.Make (Ord)
end

(** Unique identifiers for declarations in a signature.

    An ID uniquely refers to a signature declaration in a source file.
    However, since declarations may be elaborated in steps, derived
    declarations share the same ID.

    IDs are allocated during signature reconstruction. *)
module type ID = sig
  (** The type of identifiers for signature declarations. *)
  type t

  (** {1 Instances} *)

  include Ord.ORD with type t := t

  include Hash.HASH with type t := t

  (** {1 Collections} *)

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  module Hamt : Hamt.S with type key = t
end

(** Base implementation for IDs as integers. *)
module BaseId : sig
  (** Unique identifiers for declarations in a signature as integers.

      This module type enables internal modules to construct IDs as integers.
      The type for IDs remains abstract in exported module signatures. *)
  include ID with type t = int

  (** {1 Constructors} *)

  val min_value : t

  val max_value : t

  val next : t -> t
end = struct
  include Int

  let min_value = 0

  let max_value = max_int

  let next = ( + ) 1
end

module Id = struct
  module Typ = BaseId
  module Const = BaseId
  module CompTyp = BaseId
  module CompConst = BaseId
  module CompCotyp = BaseId
  module CompDest = BaseId
  module Comp = BaseId
  module Module = BaseId
  module Query = BaseId
  module MQuery = BaseId
  module Schema = BaseId

  module Allocator = struct
    type state =
      { typ : BaseId.t
      ; const : BaseId.t
      ; comp_typ : BaseId.t
      ; comp_const : BaseId.t
      ; comp_cotyp : BaseId.t
      ; comp_dest : BaseId.t
      ; comp : BaseId.t
      ; module_ : BaseId.t
      ; query : BaseId.t
      ; mquery : BaseId.t
      ; schema : BaseId.t
      }

    include (
      State.Make (struct
        type t = state
      end) :
        State.STATE with type state := state)

    let initial_state =
      { typ = BaseId.min_value
      ; const = BaseId.min_value
      ; comp_typ = BaseId.min_value
      ; comp_const = BaseId.min_value
      ; comp_cotyp = BaseId.min_value
      ; comp_dest = BaseId.min_value
      ; comp = BaseId.min_value
      ; module_ = BaseId.min_value
      ; query = BaseId.min_value
      ; mquery = BaseId.min_value
      ; schema = BaseId.min_value
      }

    let next_id getter setter =
      get >>= fun state ->
      let previous_id = getter state in
      if BaseId.(previous_id = max_value) then
        raise @@ Invalid_argument "Exhausted sequence of fresh IDs"
      else
        let next = BaseId.next previous_id in
        put (setter state next) $> Fun.const next

    let next_typ_id =
      next_id (fun { typ; _ } -> typ) (fun state typ -> { state with typ })

    let next_const_id =
      next_id
        (fun { const; _ } -> const)
        (fun state const -> { state with const })

    let next_comp_typ_id =
      next_id
        (fun { comp_typ; _ } -> comp_typ)
        (fun state comp_typ -> { state with comp_typ })

    let next_comp_const_id =
      next_id
        (fun { comp_const; _ } -> comp_const)
        (fun state comp_const -> { state with comp_const })

    let next_comp_cotyp_id =
      next_id
        (fun { comp_cotyp; _ } -> comp_cotyp)
        (fun state comp_cotyp -> { state with comp_cotyp })

    let next_comp_dest_id =
      next_id
        (fun { comp_dest; _ } -> comp_dest)
        (fun state comp_dest -> { state with comp_dest })

    let next_comp_id =
      next_id
        (fun { comp; _ } -> comp)
        (fun state comp -> { state with comp })

    let next_module_id =
      next_id
        (fun { module_; _ } -> module_)
        (fun state module_ -> { state with module_ })

    let next_query_id =
      next_id
        (fun { query; _ } -> query)
        (fun state query -> { state with query })

    let next_mquery_id =
      next_id
        (fun { mquery; _ } -> mquery)
        (fun state mquery -> { state with mquery })

    let next_schema_id =
      next_id
        (fun { schema; _ } -> schema)
        (fun state schema -> { state with schema })
  end
end

module Syntax = struct
  module Int = struct
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
  end
end

(** Beluga Signature Entries *)

module DocumentationComment = struct
  type t =
    { content : string
    ; location : Location.t
    }

  let make ~location content = { content; location }

  let[@inline] content { content; _ } = content

  let[@inline] location { location; _ } = location
end

module Typ = struct
  open Syntax.Int

  module Kind = struct
    (** [arguments tK] is [(i, e, t)] where

        - [i] is the number of implicit arguments in [tK]
        - [e] is the number of explicit and inductive arguments in [tK]
        - [t] is the total number of implicit, explicit and inductive
          arguments in [tK] *)
    let arguments =
      let rec arguments (implicit, explicit) tK =
        match tK with
        | LF.Typ -> (implicit, explicit)
        | LF.PiKind ((_, Depend.Explicit), tK')
        | LF.PiKind ((_, Depend.Inductive), tK') ->
          arguments (implicit, 1 + explicit) tK'
        | LF.PiKind ((_, Depend.Implicit), tK') ->
          arguments (1 + implicit, explicit) tK'
      in
      fun tK ->
        let implicit, explicit = arguments (0, 0) tK in
        (implicit, explicit, implicit + explicit)
  end

  module Unfrozen = struct
    type t =
      { id : Id.Typ.t
      ; name : Name.t
      ; location : Location.t
      ; arguments : int
      ; implicit_arguments : int
      ; explicit_arguments : int
      ; kind : LF.kind
      ; var_name_base : Name.t Option.t
      ; mvar_name_base : Name.t Option.t
      ; constructors : Id.Const.t Name.Hamt.t
      ; documentation_comment : DocumentationComment.t Option.t
      }

    let make ~id ~name ~location ?var_name_base ?mvar_name_base
        ?(constructors = Name.Hamt.empty)
        ?(documentation_comment = Option.none) kind =
      let implicit_arguments, explicit_arguments, arguments =
        Kind.arguments kind
      in
      { id
      ; name
      ; location
      ; implicit_arguments
      ; explicit_arguments
      ; arguments
      ; kind
      ; var_name_base
      ; mvar_name_base
      ; constructors
      ; documentation_comment
      }

    let add_constructor ({ constructors; _ } as entry) name const =
      { entry with constructors = Name.Hamt.add name const constructors }

    let set_var_naming_convention var entry =
      { entry with var_name_base = var }

    let set_mvar_naming_convention mvar entry =
      { entry with mvar_name_base = mvar }

    let set_naming_conventions ~var ~mvar entry =
      { entry with var_name_base = var; mvar_name_base = mvar }
  end

  module Frozen = struct
    type t =
      { id : Id.Typ.t
      ; name : Name.t
      ; location : Location.t
      ; arguments : int
      ; implicit_arguments : int
      ; explicit_arguments : int
      ; kind : LF.kind
      ; var_name_base : Name.t Option.t
      ; mvar_name_base : Name.t Option.t
      ; constructors : Id.Const.t Name.Hamt.t
      ; term_subordinates : Id.Typ.Set.t
      ; type_subordinated_to : Id.Typ.Set.t
      ; documentation_comment : DocumentationComment.t Option.t
      }

    let[@inline] term_subordinates { term_subordinates; _ } =
      term_subordinates

    let[@inline] type_subordinated_to { type_subordinated_to; _ } =
      type_subordinated_to

    let set_var_naming_convention var entry =
      { entry with var_name_base = var }

    let set_mvar_naming_convention mvar entry =
      { entry with mvar_name_base = mvar }

    let set_naming_conventions ~var ~mvar entry =
      { entry with var_name_base = var; mvar_name_base = mvar }
  end

  type t =
    | Frozen of Frozen.t
    | Unfrozen of Unfrozen.t

  let[@inline] id = function
    | Frozen { Frozen.id; _ } | Unfrozen { Unfrozen.id; _ } -> id

  let[@inline] location = function
    | Frozen { Frozen.location; _ } | Unfrozen { Unfrozen.location; _ } ->
      location

  let[@inline] name = function
    | Frozen { Frozen.name; _ } | Unfrozen { Unfrozen.name; _ } -> name

  let[@inline] kind = function
    | Frozen { Frozen.kind; _ } | Unfrozen { Unfrozen.kind; _ } -> kind

  let[@inline] arguments = function
    | Frozen { Frozen.arguments; _ } | Unfrozen { Unfrozen.arguments; _ } ->
      arguments

  let[@inline] implicit_arguments = function
    | Frozen { Frozen.implicit_arguments; _ }
    | Unfrozen { Unfrozen.implicit_arguments; _ } -> implicit_arguments

  let[@inline] explicit_arguments = function
    | Frozen { Frozen.explicit_arguments; _ }
    | Unfrozen { Unfrozen.explicit_arguments; _ } -> explicit_arguments

  let[@inline] var_name_base = function
    | Frozen { Frozen.var_name_base; _ }
    | Unfrozen { Unfrozen.var_name_base; _ } -> var_name_base

  let[@inline] mvar_name_base = function
    | Frozen { Frozen.mvar_name_base; _ }
    | Unfrozen { Unfrozen.mvar_name_base; _ } -> mvar_name_base

  let[@inline] constructors = function
    | Frozen { Frozen.constructors; _ }
    | Unfrozen { Unfrozen.constructors; _ } -> constructors

  let[@inline] documentation_comment = function
    | Frozen { Frozen.documentation_comment; _ }
    | Unfrozen { Unfrozen.documentation_comment; _ } -> documentation_comment

  let make_initial_declaration ~id ~name ~location ?documentation_comment
      kind =
    Unfrozen (Unfrozen.make ~id ~name ~location ~documentation_comment kind)

  let is_frozen = function
    | Frozen _ -> true
    | Unfrozen _ -> false

  let is_unfrozen entry = not @@ is_frozen entry

  exception FrozenTyp of t

  let if_unfrozen f = function
    | Frozen _ as tA -> raise @@ FrozenTyp tA
    | Unfrozen entry -> f entry

  exception UnfrozenTyp of t

  let if_frozen f = function
    | Frozen entry -> f entry
    | Unfrozen _ as tA -> raise @@ UnfrozenTyp tA

  let has_constructor_with_name name =
    Fun.(constructors >> Name.Hamt.mem name)

  exception TypNameCollision of Name.t * Id.Const.t * t

  exception ConstNameCollision of Name.t * Id.Const.t * t

  let add_constructor tM_name tM tA =
    if Name.(name tA <> tM_name) then
      raise @@ TypNameCollision (tM_name, tM, tA);
    if has_constructor_with_name tM_name tA then
      raise @@ ConstNameCollision (tM_name, tM, tA);
    tA
    |> if_unfrozen (fun tA ->
           Unfrozen (Unfrozen.add_constructor tA tM_name tM))

  let frozen ~term_subordinates ~type_subordinated_to
      { Unfrozen.id
      ; name
      ; location
      ; arguments
      ; implicit_arguments
      ; explicit_arguments
      ; kind
      ; var_name_base
      ; mvar_name_base
      ; constructors
      ; documentation_comment
      } =
    { Frozen.id
    ; name
    ; location
    ; arguments
    ; implicit_arguments
    ; explicit_arguments
    ; kind
    ; constructors
    ; var_name_base
    ; mvar_name_base
    ; term_subordinates
    ; type_subordinated_to
    ; documentation_comment
    }

  let freeze ~term_subordinates ~type_subordinated_to =
    if_unfrozen (fun x ->
        Frozen (frozen ~term_subordinates ~type_subordinated_to x))

  let fresh_var_name entry ?(default_base_name = "x") =
    entry |> var_name_base |> Option.map Name.show
    |> Option.value ~default:default_base_name
    |> Name.prefixed_fresh_name_supplier

  let fresh_mvar_name entry ?(default_base_name = "X") =
    entry |> mvar_name_base |> Option.map Name.show
    |> Option.value ~default:default_base_name
    |> Name.prefixed_fresh_name_supplier

  let set_var_naming_convention var = function
    | Frozen x -> Frozen (Frozen.set_var_naming_convention var x)
    | Unfrozen x -> Unfrozen (Unfrozen.set_var_naming_convention var x)

  let set_mvar_naming_convention mvar = function
    | Frozen x -> Frozen (Frozen.set_mvar_naming_convention mvar x)
    | Unfrozen x -> Unfrozen (Unfrozen.set_mvar_naming_convention mvar x)

  let set_naming_conventions ~var ~mvar = function
    | Frozen x -> Frozen (Frozen.set_naming_conventions ~var ~mvar x)
    | Unfrozen x -> Unfrozen (Unfrozen.set_naming_conventions ~var ~mvar x)

  let is_term_subordinate entry typ =
    entry |> if_frozen Fun.(Frozen.term_subordinates >> Id.Typ.Set.mem typ)

  let is_type_subordinate_to entry typ =
    entry
    |> if_frozen Fun.(Frozen.type_subordinated_to >> Id.Typ.Set.mem typ)
end

module Const = struct
  open Syntax.Int

  type t =
    { id : Id.Const.t
    ; name : Name.t
    ; location : Location.t
    ; implicit_arguments : int
    ; typ : LF.typ
    ; kind : Id.Typ.t
    ; documentation_comment : DocumentationComment.t Option.t
    }

  let make ~id ~name ~location ~implicit_arguments ~kind
      ?(documentation_comment : DocumentationComment.t Option.t) typ =
    { id
    ; name
    ; location
    ; implicit_arguments
    ; typ
    ; kind
    ; documentation_comment
    }

  let[@inline] id { id; _ } = id

  let[@inline] location { location; _ } = location

  let[@inline] name { name; _ } = name

  let[@inline] typ { typ; _ } = typ

  let[@inline] kind { kind; _ } = kind

  let[@inline] documentation_comment { documentation_comment; _ } =
    documentation_comment
end

module CompTyp = struct
  open Syntax.Int

  module Unfrozen = struct
    type t =
      { id : Id.CompTyp.t
      ; name : Name.t
      ; location : Location.t
      ; implicit_arguments : int
      ; kind : Comp.kind
      ; positivity : Sgn.positivity_flag
      ; constructors : Id.CompConst.t Name.Hamt.t
      ; documentation_comment : DocumentationComment.t Option.t
      }

    let make ~id ~name ~location ~implicit_arguments ~positivity
        ?(constructors = Name.Hamt.empty)
        ?(documentation_comment = Option.none) kind =
      { id
      ; name
      ; location
      ; implicit_arguments
      ; kind
      ; positivity
      ; constructors
      ; documentation_comment
      }

    let add_constructor ({ constructors; _ } as entry) name const =
      { entry with constructors = Name.Hamt.add name const constructors }
  end

  module Frozen = struct
    type t =
      { id : Id.CompTyp.t
      ; name : Name.t
      ; location : Location.t
      ; implicit_arguments : int
      ; kind : Comp.kind
      ; positivity : Sgn.positivity_flag
      ; constructors : Id.CompConst.t Name.Hamt.t
      ; documentation_comment : DocumentationComment.t Option.t
      }
  end

  type t =
    | Frozen of Frozen.t
    | Unfrozen of Unfrozen.t

  let[@inline] id = function
    | Frozen { Frozen.id; _ } | Unfrozen { Unfrozen.id; _ } -> id

  let[@inline] location = function
    | Frozen { Frozen.location; _ } | Unfrozen { Unfrozen.location; _ } ->
      location

  let[@inline] name = function
    | Frozen { Frozen.name; _ } | Unfrozen { Unfrozen.name; _ } -> name

  let[@inline] kind = function
    | Frozen { Frozen.kind; _ } | Unfrozen { Unfrozen.kind; _ } -> kind

  let[@inline] constructors = function
    | Frozen { Frozen.constructors; _ }
    | Unfrozen { Unfrozen.constructors; _ } -> constructors

  let[@inline] documentation_comment = function
    | Frozen { Frozen.documentation_comment; _ }
    | Unfrozen { Unfrozen.documentation_comment; _ } -> documentation_comment

  let make_initial_declaration ~id ~name ~location ~implicit_arguments
      ~positivity ?documentation_comment kind =
    Unfrozen
      (Unfrozen.make ~id ~name ~location ~implicit_arguments ~positivity
         ~documentation_comment kind)

  let is_frozen = function
    | Frozen _ -> true
    | Unfrozen _ -> false

  let is_unfrozen entry = not @@ is_frozen entry

  exception FrozenCompTyp of t

  let if_unfrozen f = function
    | Frozen _ as cA -> raise @@ FrozenCompTyp cA
    | Unfrozen entry -> f entry

  exception UnfrozenCompTyp of t

  let if_frozen f = function
    | Frozen entry -> f entry
    | Unfrozen _ as cA -> raise @@ UnfrozenCompTyp cA

  let has_constructor_with_name name =
    Fun.(constructors >> Name.Hamt.mem name)

  exception CompTypNameCollision of Name.t * Id.CompConst.t * t

  exception CompConstNameCollision of Name.t * Id.CompConst.t * t

  let add_constructor cM_name cM cA =
    if Name.(name cA <> cM_name) then
      raise @@ CompTypNameCollision (cM_name, cM, cA);
    if has_constructor_with_name cM_name cA then
      raise @@ CompConstNameCollision (cM_name, cM, cA);
    cA
    |> if_unfrozen (fun x ->
           Unfrozen (Unfrozen.add_constructor x cM_name cM))

  let frozen
      { Unfrozen.id
      ; name
      ; location
      ; implicit_arguments
      ; kind
      ; positivity
      ; constructors
      ; documentation_comment
      } =
    { Frozen.id
    ; name
    ; location
    ; implicit_arguments
    ; kind
    ; positivity
    ; constructors
    ; documentation_comment
    }

  let freeze = if_unfrozen (fun x -> Frozen (frozen x))
end

module CompConst = struct
  open Syntax.Int

  type t =
    { id : Id.CompConst.t
    ; name : Name.t
    ; location : Location.t
    ; implicit_arguments : int
    ; typ : Comp.typ
    ; kind : Id.CompTyp.t
    ; documentation_comment : DocumentationComment.t Option.t
    }

  let make ~id ~name ~location ~implicit_arguments ~kind
      ?documentation_comment typ =
    { id
    ; name
    ; location
    ; implicit_arguments
    ; typ
    ; kind
    ; documentation_comment
    }

  let[@inline] id { id; _ } = id

  let[@inline] location { location; _ } = location

  let[@inline] name { name; _ } = name

  let[@inline] implicit_arguments { implicit_arguments; _ } =
    implicit_arguments

  let[@inline] typ { typ; _ } = typ

  let[@inline] kind { kind; _ } = kind

  let[@inline] documentation_comment { documentation_comment; _ } =
    documentation_comment
end

module CompCotyp = struct
  open Syntax.Int

  module Unfrozen = struct
    type t =
      { id : Id.CompCotyp.t
      ; name : Name.t
      ; location : Location.t
      ; implicit_arguments : int
      ; kind : Comp.kind
      ; destructors : Id.CompDest.t Name.Hamt.t
      ; documentation_comment : DocumentationComment.t Option.t
      }

    let make ~id ~name ~location ~implicit_arguments
        ?(destructors = Name.Hamt.empty)
        ?(documentation_comment = Option.none) kind =
      { id
      ; name
      ; location
      ; implicit_arguments
      ; kind
      ; destructors
      ; documentation_comment
      }

    let add_destructor ({ destructors; _ } as entry) name dest =
      { entry with destructors = Name.Hamt.add name dest destructors }
  end

  module Frozen = struct
    type t =
      { id : Id.CompTyp.t
      ; name : Name.t
      ; location : Location.t
      ; implicit_arguments : int
      ; kind : Comp.kind
      ; destructors : Id.CompDest.t Name.Hamt.t
      ; documentation_comment : DocumentationComment.t Option.t
      }
  end

  type t =
    | Frozen of Frozen.t
    | Unfrozen of Unfrozen.t

  let[@inline] id = function
    | Frozen { Frozen.id; _ } | Unfrozen { Unfrozen.id; _ } -> id

  let[@inline] location = function
    | Frozen { Frozen.location; _ } | Unfrozen { Unfrozen.location; _ } ->
      location

  let[@inline] name = function
    | Frozen { Frozen.name; _ } | Unfrozen { Unfrozen.name; _ } -> name

  let[@inline] kind = function
    | Frozen { Frozen.kind; _ } | Unfrozen { Unfrozen.kind; _ } -> kind

  let[@inline] destructors = function
    | Frozen { Frozen.destructors; _ } | Unfrozen { Unfrozen.destructors; _ }
      -> destructors

  let[@inline] documentation_comment = function
    | Frozen { Frozen.documentation_comment; _ }
    | Unfrozen { Unfrozen.documentation_comment; _ } -> documentation_comment

  let make_initial_declaration ~id ~name ~location ~implicit_arguments
      ?documentation_comment kind =
    Unfrozen
      (Unfrozen.make ~id ~name ~location ~implicit_arguments
         ~documentation_comment kind)

  let is_frozen = function
    | Frozen _ -> true
    | Unfrozen _ -> false

  let is_unfrozen entry = not @@ is_frozen entry

  exception FrozenCompCotyp of t

  let if_unfrozen f = function
    | Frozen _ as cA -> raise @@ FrozenCompCotyp cA
    | Unfrozen entry -> f entry

  exception UnfrozenCompCotyp of t

  let if_frozen f = function
    | Frozen entry -> f entry
    | Unfrozen _ as cA -> raise @@ UnfrozenCompCotyp cA

  let has_destructor_with_name name = Fun.(destructors >> Name.Hamt.mem name)

  exception CompCotypNameCollision of Name.t * Id.CompDest.t * t

  exception CompDestNameCollision of Name.t * Id.CompDest.t * t

  let add_destructor cM_name cM cA =
    if Name.(name cA <> cM_name) then
      raise @@ CompCotypNameCollision (cM_name, cM, cA);
    if has_destructor_with_name cM_name cA then
      raise @@ CompDestNameCollision (cM_name, cM, cA);
    cA
    |> if_unfrozen (fun x -> Unfrozen (Unfrozen.add_destructor x cM_name cM))

  let frozen
      { Unfrozen.id
      ; name
      ; location
      ; implicit_arguments
      ; kind
      ; destructors
      ; documentation_comment
      } =
    { Frozen.id
    ; name
    ; location
    ; implicit_arguments
    ; kind
    ; destructors
    ; documentation_comment
    }

  let freeze = if_unfrozen (fun x -> Frozen (frozen x))
end

module CompDest = struct
  open Syntax.Int

  type t =
    { id : Id.CompDest.t
    ; name : Name.t
    ; location : Location.t
    ; implicit_arguments : int
    ; mctx : LF.mctx
    ; observation_typ : Comp.typ
    ; return_typ : Comp.typ
    ; kind : Id.CompCotyp.t
    ; documentation_comment : DocumentationComment.t Option.t
    }

  let make ~id ~name ~location ~implicit_arguments ~mctx ~observation_typ
      ~return_typ ?documentation_comment kind =
    { id
    ; name
    ; location
    ; implicit_arguments
    ; mctx
    ; observation_typ
    ; return_typ
    ; kind
    ; documentation_comment
    }

  let[@inline] id { id; _ } = id

  let[@inline] location { location; _ } = location

  let[@inline] name { name; _ } = name

  let[@inline] implicit_arguments { implicit_arguments; _ } =
    implicit_arguments

  let[@inline] mctx { mctx; _ } = mctx

  let[@inline] observation_typ { observation_typ; _ } = observation_typ

  let[@inline] return_typ { return_typ; _ } = return_typ

  let[@inline] kind { kind; _ } = kind

  let[@inline] documentation_comment { documentation_comment; _ } =
    documentation_comment
end

module Comp = struct
  open Syntax.Int

  type t =
    { id : Id.Comp.t
    ; name : Name.t
    ; location : Location.t
    ; implicit_arguments : int
    ; typ : Comp.typ
    ; mutual_group : Id.Comp.t List1.t Option.t
    ; program : Comp.value
    ; documentation_comment : DocumentationComment.t Option.t
    }

  let make ~id ~name ~location ~implicit_arguments ~typ ?mutual_group
      ?documentation_comment program =
    { id
    ; name
    ; location
    ; implicit_arguments
    ; typ
    ; mutual_group
    ; program
    ; documentation_comment
    }

  let[@inline] id { id; _ } = id

  let[@inline] location { location; _ } = location

  let[@inline] name { name; _ } = name

  let[@inline] implicit_arguments { implicit_arguments; _ } =
    implicit_arguments

  let[@inline] typ { typ; _ } = typ

  let[@inline] program { program; _ } = program

  let[@inline] mutual_group { mutual_group; _ } = mutual_group

  let[@inline] documentation_comment { documentation_comment; _ } =
    documentation_comment
end

module Module = struct
  type ('signature, 'entry, 'declaration) t =
    { id : Id.Module.t
    ; name : Name.t
    ; location : Location.t
    ; entries : ('signature * 'entry) List.t
    ; bindings : ('signature * 'declaration) Name.Hamt.t
    ; documentation_comment : DocumentationComment.t Option.t
    }

  let make_empty ~id ~location ?documentation_comment name =
    { id
    ; name
    ; location
    ; entries = []
    ; bindings = Name.Hamt.empty
    ; documentation_comment
    }

  let add_entry ({ entries; _ } as m) entry =
    { m with entries = List.cons entry entries }

  let add_binding ({ bindings; _ } as module_) name binding =
    { module_ with
      bindings =
        bindings |> Name.Hamt.alter name (Fun.const @@ Option.some binding)
    }

  let[@inline] id { id; _ } = id

  let[@inline] location { location; _ } = location

  let[@inline] name { name; _ } = name

  let[@inline] entries { entries; _ } = entries

  let[@inline] bindings { bindings; _ } = bindings

  let[@inline] documentation_comment { documentation_comment; _ } =
    documentation_comment

  let lookup m name = m |> bindings |> Name.Hamt.find_opt name

  let rec deep_lookup extract current_module module_names base_name =
    match module_names with
    | [] -> lookup current_module base_name
    | head_module_name :: tail_module_names ->
      let open Option in
      lookup current_module head_module_name >>= extract >>= fun m' ->
      deep_lookup extract m' tail_module_names base_name

  let fold_entries f init m =
    m |> entries |> List.fold_right (Fun.flip f) |> Fun.apply init
end

module Schema = struct
  open Syntax.Int

  type t =
    { id : Id.Schema.t
    ; name : Name.t
    ; location : Location.t
    ; schema : LF.schema
    ; documentation_comment : DocumentationComment.t Option.t
    }

  let make ~id ~name ~location ?documentation_comment schema =
    { id; name; location; schema; documentation_comment }

  let[@inline] id { id; _ } = id

  let[@inline] name { name; _ } = name

  let[@inline] location { location; _ } = location

  let[@inline] schema { schema; _ } = schema

  let[@inline] documentation_comment { documentation_comment; _ } =
    documentation_comment
end

module Query = struct
  open Syntax.Int

  type search_parameters =
    { expected_solutions : int Option.t
    ; maximum_tries : int Option.t
    ; search_depth : int Option.t
    }

  let make_search_parameters ?expected_solutions ?maximum_tries ?search_depth
      () =
    { expected_solutions; maximum_tries; search_depth }

  type t =
    { id : Id.Query.t
    ; location : Location.t
    ; name : Name.t Option.t
    ; query : LF.mctx * (LF.typ * offset)
    ; search_parameters : search_parameters
    ; documentation_comment : DocumentationComment.t Option.t
    }

  let make ~id ~location ?name
      ?(search_parameters = make_search_parameters ()) ?documentation_comment
      query =
    { id; location; name; search_parameters; query; documentation_comment }

  let[@inline] id { id; _ } = id

  let[@inline] location { location; _ } = location

  let[@inline] name { name; _ } = name

  let[@inline] query { query; _ } = query

  let[@inline] search_parameters { search_parameters; _ } = search_parameters

  let[@inline] documentation_comment { documentation_comment; _ } =
    documentation_comment
end

module MQuery = struct
  open Syntax.Int

  type search_parameters =
    { expected_solutions : int Option.t
    ; search_tries : int Option.t
    ; search_depth : int Option.t
    ; split_index : int Option.t
    }

  let make_search_parameters ?expected_solutions ?search_tries ?search_depth
      ?split_index () =
    { expected_solutions; search_tries; search_depth; split_index }

  type t =
    { id : Id.MQuery.t
    ; location : Location.t
    ; name : Name.t Option.t
    ; query : Comp.typ * offset
    ; search_parameters : search_parameters
    ; documentation_comment : DocumentationComment.t Option.t
    }

  let make ~id ~location ?name
      ?(search_parameters = make_search_parameters ()) ?documentation_comment
      query =
    { id; location; name; search_parameters; query; documentation_comment }

  let[@inline] id { id; _ } = id

  let[@inline] location { location; _ } = location

  let[@inline] name { name; _ } = name

  let[@inline] query { query; _ } = query

  let[@inline] search_parameters { search_parameters; _ } = search_parameters

  let[@inline] documentation_comment { documentation_comment; _ } =
    documentation_comment
end

module NamePragma = struct
  type t =
    { location : Location.t
    ; var_naming_convention : string Option.t
    ; mvar_naming_convention : string
    ; typ : Id.Typ.t
    }

  let make ~location ~var_naming_convention ~mvar_naming_convention ~typ =
    { location; var_naming_convention; mvar_naming_convention; typ }

  let[@inline] location { location; _ } = location

  let[@inline] var_naming_convention { var_naming_convention; _ } =
    var_naming_convention

  let[@inline] mvar_naming_convention { mvar_naming_convention; _ } =
    mvar_naming_convention

  let[@inline] typ { typ; _ } = typ
end

(** Beluga Signatures *)

type mutually_recursive_typs =
  [ `Typs of (Typ.t * Const.t Name.LinkedHamt.t) List1.t ]

type mutually_recursive_comp_typs =
  [ `Comp_typs of
    [ `Comp_typ of CompTyp.t * CompConst.t Name.LinkedHamt.t
    | `Comp_cotyp of CompCotyp.t * CompDest.t Name.LinkedHamt.t
    ]
    List1.t
  ]

type mutually_recursive_programs = [ `Programs of Comp.t Name.LinkedHamt1.t ]

type entry =
  [ `Typ_declaration of Typ.t
  | `Const_declaration of Const.t
  | `Comp_typ_declaration of CompTyp.t
  | `Comp_const_declaration of CompConst.t
  | `Comp_cotyp_declaration of CompCotyp.t
  | `Comp_dest_declaration of CompDest.t
  | `Comp_declaration of Comp.t
  | `Schema_declaration of Schema.t
  | `Module_declaration of (t, entry, declaration) Module.t
  | `Documentation_comment of DocumentationComment.t
  | `Mutually_recursive_declaration of
    [ mutually_recursive_typs
    | mutually_recursive_comp_typs
    | mutually_recursive_programs
    ]
  | `Query_declaration of Query.t
  | `MQuery_declaration of MQuery.t
  | `Name_pragma of NamePragma.t
  ]

and declaration =
  [ `Typ_declaration of Typ.t
  | `Const_declaration of Const.t
  | `Comp_typ_declaration of CompTyp.t
  | `Comp_const_declaration of CompConst.t
  | `Comp_cotyp_declaration of CompCotyp.t
  | `Comp_dest_declaration of CompDest.t
  | `Comp_declaration of Comp.t
  | `Schema_declaration of Schema.t
  | `Module_declaration of (t, entry, declaration) Module.t
  | `Query_declaration of Query.t
  | `MQuery_declaration of MQuery.t
  ]

and t =
  { entries : (t Lazy.t * entry) List.t
  ; bindings : (t Lazy.t * declaration) Name.Hamt.t
  ; typs : (t Lazy.t * Typ.t) Id.Typ.Hamt.t
  ; consts : (t Lazy.t * Const.t) Id.Const.Hamt.t
  ; comp_typs : (t Lazy.t * CompTyp.t) Id.CompTyp.Hamt.t
  ; comp_consts : (t Lazy.t * CompConst.t) Id.CompConst.Hamt.t
  ; comp_cotyps : (t Lazy.t * CompCotyp.t) Id.CompCotyp.Hamt.t
  ; comp_dests : (t Lazy.t * CompDest.t) Id.CompDest.Hamt.t
  ; comps : (t Lazy.t * Comp.t) Id.Comp.Hamt.t
  ; modules : (t Lazy.t * (t, entry, declaration) Module.t) Id.Module.Hamt.t
  ; schemas : (t Lazy.t * Schema.t) Id.Schema.Hamt.t
  ; queries : (t Lazy.t * Query.t) Id.Query.Hamt.t
  ; mqueries : (t Lazy.t * MQuery.t) Id.MQuery.Hamt.t
  ; unfrozen_typs : Id.Typ.Set.t
  ; unfrozen_comp_typs : Id.CompTyp.Set.t
  ; unfrozen_comp_cotyps : Id.CompCotyp.Set.t
  }

type signature = t

(** Destructors *)

let[@inline] entries { entries; _ } = entries

let[@inline] bindings { bindings; _ } = bindings

let[@inline] typs { typs; _ } = typs

let[@inline] consts { consts; _ } = consts

let[@inline] comp_typs { comp_typs; _ } = comp_typs

let[@inline] comp_consts { comp_consts; _ } = comp_consts

let[@inline] comp_cotyps { comp_cotyps; _ } = comp_cotyps

let[@inline] comp_dests { comp_dests; _ } = comp_dests

let[@inline] comps { comps; _ } = comps

let[@inline] modules { modules; _ } = modules

let[@inline] schemas { schemas; _ } = schemas

let[@inline] queries { queries; _ } = queries

let[@inline] mqueries { mqueries; _ } = mqueries

let[@inline] unfrozen_typs { unfrozen_typs; _ } = unfrozen_typs

let[@inline] unfrozen_comp_typs { unfrozen_comp_typs; _ } =
  unfrozen_comp_typs

let[@inline] unfrozen_comp_cotyps { unfrozen_comp_cotyps; _ } =
  unfrozen_comp_cotyps

(** Declaration Guards *)

let guard_typ_declaration : [> `Typ_declaration of Typ.t ] -> Typ.t Option.t
    = function
  | `Typ_declaration declaration -> Option.some declaration
  | _ -> Option.none

let guard_const_declaration :
    [> `Const_declaration of Const.t ] -> Const.t Option.t = function
  | `Const_declaration declaration -> Option.some declaration
  | _ -> Option.none

let guard_comp_typ_declaration :
    [> `Comp_typ_declaration of CompTyp.t ] -> CompTyp.t Option.t = function
  | `Comp_typ_declaration declaration -> Option.some declaration
  | _ -> Option.none

let guard_comp_const_declaration :
    [> `Comp_const_declaration of CompConst.t ] -> CompConst.t Option.t =
  function
  | `Comp_const_declaration declaration -> Option.some declaration
  | _ -> Option.none

let guard_comp_cotyp_declaration :
    [> `Comp_cotyp_declaration of CompCotyp.t ] -> CompCotyp.t Option.t =
  function
  | `Comp_cotyp_declaration declaration -> Option.some declaration
  | _ -> Option.none

let guard_comp_dest_declaration :
    [> `Comp_dest_declaration of CompDest.t ] -> CompDest.t Option.t =
  function
  | `Comp_dest_declaration declaration -> Option.some declaration
  | _ -> Option.none

let guard_comp_declaration :
    [> `Comp_declaration of Comp.t ] -> Comp.t Option.t = function
  | `Comp_declaration declaration -> Option.some declaration
  | _ -> Option.none

let guard_schema_declaration :
    [> `Schema_declaration of Schema.t ] -> Schema.t Option.t = function
  | `Schema_declaration declaration -> Option.some declaration
  | _ -> Option.none

let guard_module_declaration :
       [> `Module_declaration of ('signature, 'entry, 'declaration) Module.t ]
    -> ('signature, 'entry, 'declaration) Module.t Option.t = function
  | `Module_declaration declaration -> Option.some declaration
  | _ -> Option.none

let guard_query_declaration :
    [> `Query_declaration of Query.t ] -> Query.t Option.t = function
  | `Query_declaration query -> Option.some query
  | _ -> Option.none

let guard_mquery_declaration :
    [> `MQuery_declaration of MQuery.t ] -> MQuery.t Option.t = function
  | `MQuery_declaration mquery -> Option.some mquery
  | _ -> Option.none

let extract_declaration guard (signature, declaration_opt) =
  let open Option in
  declaration_opt |> guard $> Pair.left signature

(** Lookups *)

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

let lookup_typ_by_id_opt' signature id =
  let open Option in
  typs signature |> Id.Typ.Hamt.find_opt id $> Pair.lmap Lazy.force

let lookup_const_by_id_opt' signature id =
  let open Option in
  consts signature |> Id.Const.Hamt.find_opt id $> Pair.lmap Lazy.force

let lookup_comp_typ_by_id_opt' signature id =
  let open Option in
  comp_typs signature |> Id.CompTyp.Hamt.find_opt id $> Pair.lmap Lazy.force

let lookup_comp_const_by_id_opt' signature id =
  let open Option in
  comp_consts signature
  |> Id.CompConst.Hamt.find_opt id
  $> Pair.lmap Lazy.force

let lookup_comp_cotyp_by_id_opt' signature id =
  let open Option in
  comp_cotyps signature
  |> Id.CompCotyp.Hamt.find_opt id
  $> Pair.lmap Lazy.force

let lookup_comp_dest_by_id_opt' signature id =
  let open Option in
  comp_dests signature
  |> Id.CompDest.Hamt.find_opt id
  $> Pair.lmap Lazy.force

let lookup_comp_by_id_opt' signature id =
  let open Option in
  comps signature |> Id.Comp.Hamt.find_opt id $> Pair.lmap Lazy.force

let lookup_schema_by_id_opt' signature id =
  let open Option in
  schemas signature |> Id.Schema.Hamt.find_opt id $> Pair.lmap Lazy.force

let lookup_module_by_id_opt' signature id =
  let open Option in
  modules signature |> Id.Module.Hamt.find_opt id $> Pair.lmap Lazy.force

let lookup_query_by_id_opt' signature id =
  let open Option in
  queries signature |> Id.Query.Hamt.find_opt id $> Pair.lmap Lazy.force

let lookup_mquery_by_id_opt' signature id =
  let open Option in
  mqueries signature |> Id.MQuery.Hamt.find_opt id $> Pair.lmap Lazy.force

let extract_entry_from_lookup_opt' lookup signature id =
  let open Option in
  lookup signature id $> Pair.snd

let lookup_typ_by_id_opt =
  extract_entry_from_lookup_opt' lookup_typ_by_id_opt'

let lookup_const_by_id_opt =
  extract_entry_from_lookup_opt' lookup_const_by_id_opt'

let lookup_comp_typ_by_id_opt =
  extract_entry_from_lookup_opt' lookup_comp_typ_by_id_opt'

let lookup_comp_const_by_id_opt =
  extract_entry_from_lookup_opt' lookup_comp_const_by_id_opt'

let lookup_comp_cotyp_by_id_opt =
  extract_entry_from_lookup_opt' lookup_comp_cotyp_by_id_opt'

let lookup_comp_dest_by_id_opt =
  extract_entry_from_lookup_opt' lookup_comp_dest_by_id_opt'

let lookup_comp_by_id_opt =
  extract_entry_from_lookup_opt' lookup_comp_by_id_opt'

let lookup_schema_by_id_opt =
  extract_entry_from_lookup_opt' lookup_schema_by_id_opt'

let lookup_module_by_id_opt =
  extract_entry_from_lookup_opt' lookup_module_by_id_opt'

let lookup_query_by_id_opt =
  extract_entry_from_lookup_opt' lookup_query_by_id_opt'

let lookup_mquery_by_id_opt =
  extract_entry_from_lookup_opt' lookup_mquery_by_id_opt'

let lookup_typ_by_id' signature id =
  lookup_typ_by_id_opt' signature id
  |> Option.get_or_else (fun () -> raise @@ UnboundTypId (id, signature))

let lookup_const_by_id' signature id =
  lookup_const_by_id_opt' signature id
  |> Option.get_or_else (fun () -> raise @@ UnboundConstId (id, signature))

let lookup_comp_typ_by_id' signature id =
  lookup_comp_typ_by_id_opt' signature id
  |> Option.get_or_else (fun () -> raise @@ UnboundCompTypId (id, signature))

let lookup_comp_const_by_id' signature id =
  lookup_comp_const_by_id_opt' signature id
  |> Option.get_or_else (fun () ->
         raise @@ UnboundCompConstId (id, signature))

let lookup_comp_cotyp_by_id' signature id =
  lookup_comp_cotyp_by_id_opt' signature id
  |> Option.get_or_else (fun () ->
         raise @@ UnboundCompCotypId (id, signature))

let lookup_comp_dest_by_id' signature id =
  lookup_comp_dest_by_id_opt' signature id
  |> Option.get_or_else (fun () ->
         raise @@ UnboundCompDestId (id, signature))

let lookup_comp_by_id' signature id =
  lookup_comp_by_id_opt' signature id
  |> Option.get_or_else (fun () -> raise @@ UnboundCompId (id, signature))

let lookup_schema_by_id' signature id =
  lookup_schema_by_id_opt' signature id
  |> Option.get_or_else (fun () -> raise @@ UnboundSchemaId (id, signature))

let lookup_module_by_id' signature id =
  lookup_module_by_id_opt' signature id
  |> Option.get_or_else (fun () -> raise @@ UnboundModuleId (id, signature))

let lookup_query_by_id' signature id =
  lookup_query_by_id_opt' signature id
  |> Option.get_or_else (fun () -> raise @@ UnboundQueryId (id, signature))

let lookup_mquery_by_id' signature id =
  lookup_mquery_by_id_opt' signature id
  |> Option.get_or_else (fun () -> raise @@ UnboundMQueryId (id, signature))

let extract_entry_from_lookup_by_id' lookup signature id =
  Pair.snd @@ lookup signature id

let lookup_typ_by_id = extract_entry_from_lookup_by_id' lookup_typ_by_id'

let lookup_const_by_id = extract_entry_from_lookup_by_id' lookup_const_by_id'

let lookup_comp_typ_by_id =
  extract_entry_from_lookup_by_id' lookup_comp_typ_by_id'

let lookup_comp_const_by_id =
  extract_entry_from_lookup_by_id' lookup_comp_const_by_id'

let lookup_comp_cotyp_by_id =
  extract_entry_from_lookup_by_id' lookup_comp_cotyp_by_id'

let lookup_comp_dest_by_id =
  extract_entry_from_lookup_by_id' lookup_comp_dest_by_id'

let lookup_comp_by_id = extract_entry_from_lookup_by_id' lookup_comp_by_id'

let lookup_schema_by_id =
  extract_entry_from_lookup_by_id' lookup_schema_by_id'

let lookup_module_by_id =
  extract_entry_from_lookup_by_id' lookup_module_by_id'

let lookup_query_by_id = extract_entry_from_lookup_by_id' lookup_query_by_id'

let lookup_mquery_by_id =
  extract_entry_from_lookup_by_id' lookup_mquery_by_id'

let lookup_name' : t -> Name.t -> (t * declaration) Option.t =
 fun signature name ->
  let open Option in
  signature |> bindings |> Name.Hamt.find_opt name $> Pair.lmap Lazy.force

let lookup_name : t -> Name.t -> declaration Option.t =
 fun signature name ->
  let open Option in
  lookup_name' signature name $> Pair.snd

let lookup_opt' signature qualified_name =
  let base_name = QualifiedName.name qualified_name in
  match QualifiedName.modules qualified_name with
  | [] ->
    (* Lookup top-level declaration in signature *)
    lookup_name' signature base_name
  | head_module_name :: tail_module_names ->
    (* Lookup recursively in modules *)
    let open Option in
    head_module_name |> lookup_name' signature $> Pair.snd
    >>= guard_module_declaration
    >>= fun top_module ->
    Module.deep_lookup
      Fun.(Pair.snd >> guard_module_declaration)
      top_module tail_module_names base_name

let lookup_opt signature qualified_name =
  let open Option in
  lookup_opt' signature qualified_name $> Pair.snd

let guarded_declaration_lookup guard signature qualified_name =
  let open Option in
  lookup_opt' signature qualified_name >>= extract_declaration guard

let lookup_typ_opt' = guarded_declaration_lookup guard_typ_declaration

let lookup_const_opt' = guarded_declaration_lookup guard_const_declaration

let lookup_comp_typ_opt' =
  guarded_declaration_lookup guard_comp_typ_declaration

let lookup_comp_const_opt' =
  guarded_declaration_lookup guard_comp_const_declaration

let lookup_comp_cotyp_opt' =
  guarded_declaration_lookup guard_comp_cotyp_declaration

let lookup_comp_dest_opt' =
  guarded_declaration_lookup guard_comp_dest_declaration

let lookup_comp_opt' = guarded_declaration_lookup guard_comp_declaration

let lookup_schema_opt' = guarded_declaration_lookup guard_schema_declaration

let lookup_module_opt' = guarded_declaration_lookup guard_module_declaration

let lookup_query_opt' = guarded_declaration_lookup guard_query_declaration

let lookup_mquery_opt' = guarded_declaration_lookup guard_mquery_declaration

let extract_entry_from_lookup_opt' lookup signature id =
  let open Option in
  lookup signature id $> Pair.snd

let lookup_typ_opt = extract_entry_from_lookup_opt' lookup_typ_opt'

let lookup_const_opt = extract_entry_from_lookup_opt' lookup_const_opt'

let lookup_comp_typ_opt = extract_entry_from_lookup_opt' lookup_comp_typ_opt'

let lookup_comp_const_opt =
  extract_entry_from_lookup_opt' lookup_comp_const_opt'

let lookup_comp_cotyp_opt =
  extract_entry_from_lookup_opt' lookup_comp_cotyp_opt'

let lookup_comp_dest_opt =
  extract_entry_from_lookup_opt' lookup_comp_dest_opt'

let lookup_comp_opt = extract_entry_from_lookup_opt' lookup_comp_opt'

let lookup_schema_opt = extract_entry_from_lookup_opt' lookup_schema_opt'

let lookup_module_opt = extract_entry_from_lookup_opt' lookup_module_opt'

let lookup_query_opt = extract_entry_from_lookup_opt' lookup_query_opt'

let lookup_mquery_opt = extract_entry_from_lookup_opt' lookup_mquery_opt'

let lookup' signature qualified_name =
  lookup_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundDeclaration (qualified_name, signature))

let lookup_typ' signature qualified_name =
  lookup_typ_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundTyp (qualified_name, signature))

let lookup_const' signature qualified_name =
  lookup_const_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundConst (qualified_name, signature))

let lookup_comp_typ' signature qualified_name =
  lookup_comp_typ_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundCompTyp (qualified_name, signature))

let lookup_comp_const' signature qualified_name =
  lookup_comp_const_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundCompConst (qualified_name, signature))

let lookup_comp_cotyp' signature qualified_name =
  lookup_comp_cotyp_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundCompCotyp (qualified_name, signature))

let lookup_comp_dest' signature qualified_name =
  lookup_comp_dest_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundCompDest (qualified_name, signature))

let lookup_comp' signature qualified_name =
  lookup_comp_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundComp (qualified_name, signature))

let lookup_schema' signature qualified_name =
  lookup_schema_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundSchema (qualified_name, signature))

let lookup_module' signature qualified_name =
  lookup_module_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundModule (qualified_name, signature))

let lookup_query' signature qualified_name =
  lookup_query_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundQuery (qualified_name, signature))

let lookup_mquery' signature qualified_name =
  lookup_mquery_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundMQuery (qualified_name, signature))

let extract_entry_from_lookup' lookup signature id =
  Pair.snd @@ lookup signature id

let lookup = extract_entry_from_lookup' lookup'

let lookup_typ = extract_entry_from_lookup' lookup_typ'

let lookup_const = extract_entry_from_lookup' lookup_const'

let lookup_comp_typ = extract_entry_from_lookup' lookup_comp_typ'

let lookup_comp_const = extract_entry_from_lookup' lookup_comp_const'

let lookup_comp_cotyp = extract_entry_from_lookup' lookup_comp_cotyp'

let lookup_comp_dest = extract_entry_from_lookup' lookup_comp_dest'

let lookup_comp = extract_entry_from_lookup' lookup_comp'

let lookup_schema = extract_entry_from_lookup' lookup_schema'

let lookup_module = extract_entry_from_lookup' lookup_module'

let lookup_query = extract_entry_from_lookup' lookup_query'

let lookup_mquery = extract_entry_from_lookup' lookup_mquery'

module Subordination = struct
  open Syntax.Int

  (** The type of subordination state looked up from the signature. *)
  type old_subordinations =
    { lookup_kind : Id.Typ.t -> LF.kind
          (** [lookup_kind tA_id] is the kind corresponding to the LF family
              having ID [tA_id]. *)
    ; lookup_constructors : Id.Typ.t -> LF.typ list
          (** [lookup_constructors tA_id] are the constructors corresponding
              to the LF family having ID [tA_id]. *)
    ; is_term_subordinate_known : Id.Typ.t -> Id.Typ.t -> bool Option.t
          (** [is_term_subordinate_known tA tB] is

              - [Some true] if [tB] is a known term-level subordinate to
                [tA],
              - [Some false] if [tB] is known not to be a term-level
                subordinate to [tA],
              - [None] if the term-level subordination of [tA] and [tB] is
                unknown.

              This function is constructed from an existing Beluga signature
              when the initial subordination state is created with
              {!initial_state}. *)
    ; is_type_subordinate_to_known : Id.Typ.t -> Id.Typ.t -> bool Option.t
          (** [is_type_subordinate_to_known tA tB] is

              - [Some true] if [tB] is a known type-level subordinate to
                [tA],
              - [Some false] if [tB] is known not to be a type-level
                subordinate to [tA],
              - [None] if the type-level subordination of [tA] and [tB] is
                unknown.

              This function is constructed from an existing Beluga signature
              when the initial subordination state is created with
              {!initial_state}. *)
    }

  (** The type of newly discovered subordination state. *)
  type new_subordinations =
    { new_term_subordinations : Id.Typ.Set.t Id.Typ.Hamt.t
          (** The mapping from LF families to their term-level subordinates.*)
    ; new_type_subordinations : Id.Typ.Set.t Id.Typ.Hamt.t
          (** The mapping from LF families to their type-level subordinates. *)
    }

  type state = old_subordinations * new_subordinations

  include (
    State.Make (struct
      type t = state
    end) :
      State.STATE with type state := state)

  let[@inline] lookup_kind ({ lookup_kind; _ }, _) = lookup_kind

  let[@inline] lookup_constructors ({ lookup_constructors; _ }, _) =
    lookup_constructors

  let[@inline] is_term_subordinate_known ({ is_term_subordinate_known; _ }, _)
      =
    is_term_subordinate_known

  let[@inline] is_type_subordinate_to_known
      ({ is_type_subordinate_to_known; _ }, _) =
    is_type_subordinate_to_known

  let[@inline] new_term_subordinations (_, { new_term_subordinations; _ }) =
    new_term_subordinations

  let[@inline] new_type_subordinations (_, { new_type_subordinations; _ }) =
    new_type_subordinations

  let initial_state :
         lookup_kind:(Id.Typ.t -> LF.kind)
      -> lookup_constructors:(Id.Typ.t -> LF.typ list)
      -> is_term_subordinate_known:(Id.Typ.t -> Id.Typ.t -> bool Option.t)
      -> is_type_subordinate_to_known:(Id.Typ.t -> Id.Typ.t -> bool Option.t)
      -> state =
   fun ~lookup_kind ~lookup_constructors ~is_term_subordinate_known
       ~is_type_subordinate_to_known ->
    ( { lookup_kind
      ; lookup_constructors
      ; is_term_subordinate_known
      ; is_type_subordinate_to_known
      }
    , { new_term_subordinations = Id.Typ.Hamt.empty
      ; new_type_subordinations = Id.Typ.Hamt.empty
      } )

  let lookup_kind tA = get $> Fun.(lookup_kind >> Fun.apply tA)

  let lookup_constructors tA =
    get $> Fun.(lookup_constructors >> Fun.apply tA)

  let lookup_old_term_subordinations tA tB =
    get $> Fun.(is_term_subordinate_known >> Fun.apply tA >> Fun.apply tB)

  let lookup_old_type_subordinations tA tB =
    get $> Fun.(is_type_subordinate_to_known >> Fun.apply tA >> Fun.apply tB)

  let lookup_new_term_subordinations tA =
    get $> Fun.(new_term_subordinations >> Id.Typ.Hamt.find_opt tA)

  let lookup_new_type_subordinations tA =
    get $> Fun.(new_type_subordinations >> Id.Typ.Hamt.find_opt tA)

  (** [lookup_is_term_subordinate tA tB state] is

      - [Some true] if [tB] is a term-level subordinate to [tA] with respect
        to [state],
      - [Some false] if [tB] is not a term-level subordinate to [tA] with
        respect to [state],
      - [None] if the term-level subordination of [tA] and [tB] is unknown
        with respect to [state].

      The term-level subordination of [tA] and [tB] is determined by first
      looking up in the new subordination relations and then the old ones. *)
  let lookup_is_term_subordinate tA tB =
    lookup_new_term_subordinations tA
    >>= Option.eliminate
          (fun () -> lookup_old_term_subordinations tA tB)
          Fun.(Id.Typ.Set.mem tB >> Option.some >> return)

  (** [lookup_is_type_subordinate tA tB state] is

      - [Some true] if [tB] is a type-level subordinate to [tA] with respect
        to [state],
      - [Some false] if [tB] is not a type-level subordinate to [tA] with
        respect to [state],
      - [None] if the type-level subordination of [tA] and [tB] is unknown
        with respect to [state].

      The type-level subordination of [tA] and [tB] is determined by first
      looking up in the new subordination relations and then the old ones. *)
  let lookup_is_type_subordinate_to tA tB =
    lookup_new_type_subordinations tA
    >>= Option.eliminate
          (fun () -> lookup_old_type_subordinations tA tB)
          Fun.(Id.Typ.Set.mem tB >> Option.some >> return)

  let compute_subordinations : Id.Typ.t -> state -> new_subordinations =
    let compute_subordinations : Id.Typ.t -> new_subordinations t =
     fun _ _ -> Error.not_implemented' "[compute_subordinations tA state]"
    in
    fun tA state -> run ~init:state (compute_subordinations tA) |> Pair.snd
end

let empty_subordination_state : t -> Subordination.state =
 fun signature ->
  let lookup_kind = Fun.(lookup_typ_by_id signature >> Typ.kind) in
  let lookup_constructors =
    Fun.(
      lookup_typ_by_id signature
      >> Typ.constructors >> Name.Hamt.values
      >> List.map (lookup_const_by_id signature >> Const.typ))
  in
  let is_term_subordinate_known tA_id tB_id =
    let tA = lookup_typ_by_id signature tA_id in
    try Option.some @@ Typ.is_term_subordinate tA tB_id
    with Typ.UnfrozenTyp _ -> Option.none
  in
  let is_type_subordinate_to_known tA_id tB_id =
    let tA = lookup_typ_by_id signature tA_id in
    try Option.some @@ Typ.is_type_subordinate_to tA tB_id
    with Typ.UnfrozenTyp _ -> Option.none
  in
  Subordination.initial_state ~lookup_kind ~lookup_constructors
    ~is_term_subordinate_known ~is_type_subordinate_to_known

module Mutation = struct
  (** The type of mutations to a signature.

      Mutations may lazily refer to the signature resulting from applying the
      current mutation. Mutations must not force their second argument since
      in {!val:Mutation.apply}, that argument is analogous to a null
      reference. *)
  type t = signature -> signature Lazy.t -> signature

  type mutation = t

  (** [identity] performs no mutation on the input signature. *)
  let identity : mutation = fun signature _ -> signature

  (** [sequence_list mutations] constructs the mutation that performs the
      mutations in [mutations] in order. *)
  let sequence_list : mutation List.t -> mutation =
   fun mutations signature signature' ->
    List.fold_left
      (fun signature mutation -> mutation signature signature')
      signature mutations

  (** [sequence_seq mutations] constructs the mutation that sequentially
      performs the mutations in [mutations]. *)
  let sequence_seq : mutation Seq.t -> mutation =
   fun mutations signature signature' ->
    Seq.fold_left
      (fun signature mutation -> mutation signature signature')
      signature mutations

  (** [apply signature mutation] calls [mutation] on [signature] and on the
      mutation result recursively. *)
  let apply : signature -> mutation -> signature =
   fun signature mutation ->
    let rec signature' = lazy (mutation signature signature') in
    Lazy.force signature'

  (** [apply_list signature mutations] sequences the mutations [mutations]
      and applies them on [signature]. *)
  let apply_list : signature -> mutation List.t -> signature =
   fun signature mutations -> sequence_list mutations |> apply signature

  let add_entry : entry -> mutation =
   fun entry signature signature' ->
    { signature with
      entries = entries signature |> List.cons (signature', entry)
    }

  let add_binding : Name.t -> declaration -> mutation =
   fun name declaration signature signature' ->
    { signature with
      bindings =
        bindings signature
        |> Name.Hamt.alter name
             (Fun.const @@ Option.some (signature', declaration))
    }

  let add_binding_opt : Name.t Option.t -> declaration -> mutation =
   fun name_opt declaration signature signature' ->
    match name_opt with
    | None -> identity signature signature'
    | Some name -> add_binding name declaration signature signature'

  let add_typ : Typ.t -> mutation =
   fun tA signature signature' ->
    { signature with
      typs = Id.Typ.Hamt.add (Typ.id tA) (signature', tA) (typs signature)
    }

  let add_const : Const.t -> mutation =
   fun tM signature signature' ->
    { signature with
      consts =
        Id.Const.Hamt.add (Const.id tM) (signature', tM) (consts signature)
    }

  let add_comp_typ : CompTyp.t -> mutation =
   fun cA signature signature' ->
    { signature with
      comp_typs =
        Id.CompTyp.Hamt.add (CompTyp.id cA) (signature', cA)
          (comp_typs signature)
    }

  let add_comp_const : CompConst.t -> mutation =
   fun cM signature signature' ->
    { signature with
      comp_consts =
        Id.CompConst.Hamt.add (CompConst.id cM) (signature', cM)
          (comp_consts signature)
    }

  let add_comp_cotyp : CompCotyp.t -> mutation =
   fun cA signature signature' ->
    { signature with
      comp_cotyps =
        Id.CompCotyp.Hamt.add (CompCotyp.id cA) (signature', cA)
          (comp_cotyps signature)
    }

  let add_comp_dest : CompDest.t -> mutation =
   fun cM signature signature' ->
    { signature with
      comp_dests =
        Id.CompDest.Hamt.add (CompDest.id cM) (signature', cM)
          (comp_dests signature)
    }

  let add_comp : Comp.t -> mutation =
   fun p signature signature' ->
    { signature with
      comps = Id.Comp.Hamt.add (Comp.id p) (signature', p) (comps signature)
    }

  let add_module : (signature, entry, declaration) Module.t -> mutation =
   fun m signature signature' ->
    { signature with
      modules =
        Id.Module.Hamt.add (Module.id m) (signature', m) (modules signature)
    }

  let add_query : Query.t -> mutation =
   fun query signature signature' ->
    { signature with
      queries =
        Id.Query.Hamt.add (Query.id query) (signature', query)
          (queries signature)
    }

  let add_mquery : MQuery.t -> mutation =
   fun mquery signature signature' ->
    { signature with
      mqueries =
        Id.MQuery.Hamt.add (MQuery.id mquery) (signature', mquery)
          (mqueries signature)
    }

  let add_schema : Schema.t -> mutation =
   fun schema signature signature' ->
    { signature with
      schemas =
        Id.Schema.Hamt.add (Schema.id schema) (signature', schema)
          (schemas signature)
    }

  let add_unfrozen_typ_if : bool -> Id.Typ.t -> mutation =
   fun cond id signature signature' ->
    if cond then
      { signature with
        unfrozen_typs = Id.Typ.Set.add id (unfrozen_typs signature)
      }
    else identity signature signature'

  let add_unfrozen_comp_typ_if : bool -> Id.CompTyp.t -> mutation =
   fun cond id signature signature' ->
    if cond then
      { signature with
        unfrozen_comp_typs =
          Id.CompTyp.Set.add id (unfrozen_comp_typs signature)
      }
    else identity signature signature'

  let add_unfrozen_comp_cotyp_if : bool -> Id.CompCotyp.t -> mutation =
   fun cond id signature signature' ->
    if cond then
      { signature with
        unfrozen_comp_cotyps =
          Id.CompCotyp.Set.add id (unfrozen_comp_cotyps signature)
      }
    else identity signature signature'

  let update_typ : Typ.t -> mutation =
   fun tA signature signature' ->
    { signature with
      typs =
        Id.Typ.Hamt.alter (Typ.id tA)
          (Fun.const @@ Option.some (signature', tA))
          (typs signature)
    }

  let update_typs : Typ.t Id.Typ.Hamt.t -> mutation =
   fun tAs signature signature' ->
    { signature with
      typs =
        Id.Typ.Hamt.merge
          (fun _ tA' tA ->
            Option.alt Option.(tA' $> Pair.left signature') tA)
          tAs (typs signature)
    }

  let update_comp_typ : CompTyp.t -> mutation =
   fun cA signature signature' ->
    { signature with
      comp_typs =
        Id.Typ.Hamt.alter (CompTyp.id cA)
          (Fun.const @@ Option.some (signature', cA))
          (comp_typs signature)
    }

  let update_comp_typs : CompTyp.t Id.Typ.Hamt.t -> mutation =
   fun cAs signature signature' ->
    { signature with
      comp_typs =
        Id.CompTyp.Hamt.merge
          (fun _ cA' cA ->
            Option.alt Option.(cA' $> Pair.left signature') cA)
          cAs (comp_typs signature)
    }

  let update_comp_cotyp : CompCotyp.t -> mutation =
   fun cA signature signature' ->
    { signature with
      comp_cotyps =
        Id.Typ.Hamt.alter (CompCotyp.id cA)
          (Fun.const @@ Option.some (signature', cA))
          (comp_cotyps signature)
    }

  let update_comp_cotyps : CompCotyp.t Id.Typ.Hamt.t -> mutation =
   fun cAs signature signature' ->
    { signature with
      comp_cotyps =
        Id.CompTyp.Hamt.merge
          (fun _ cA' cA ->
            Option.alt Option.(cA' $> Pair.left signature') cA)
          cAs (comp_cotyps signature)
    }

  let freeze_typs : Id.Typ.Set.t -> mutation =
   fun tAs signature _ ->
    { signature with
      unfrozen_typs = Id.Typ.Set.diff (unfrozen_typs signature) tAs
    }

  let freeze_comp_typs : Id.CompTyp.Set.t -> mutation =
   fun cAs signature _ ->
    { signature with
      unfrozen_comp_typs = Id.Typ.Set.diff (unfrozen_comp_typs signature) cAs
    }

  let freeze_comp_cotyps : Id.CompTyp.Set.t -> mutation =
   fun cAs signature _ ->
    { signature with
      unfrozen_comp_cotyps =
        Id.Typ.Set.diff (unfrozen_comp_cotyps signature) cAs
    }

  (** [freeze_typ tA] is the mutation that freezes at least the LF family
      declaration having ID [tA] only if it is unfrozen. If that declaration
      is already frozen, then the input signature is returned as is.

      The term-level and type-level subordination relations for [tA] are
      computed in the process, which may cause other LF family declarations
      to be frozen as well. *)
  let freeze_typ : Typ.t -> mutation =
   fun tA signature signature' ->
    if Typ.is_frozen tA then signature
    else
      let { Subordination.new_term_subordinations; new_type_subordinations }
          =
        Subordination.compute_subordinations (Typ.id tA)
          (empty_subordination_state signature)
      in
      let replacements =
        Id.Typ.Hamt.merge
          (fun tA_id term_subordinations type_subordinations ->
            Option.some
            @@ Typ.freeze
                 ~term_subordinates:
                   (Option.value ~default:Id.Typ.Set.empty
                      term_subordinations)
                 ~type_subordinated_to:
                   (Option.value ~default:Id.Typ.Set.empty
                      type_subordinations)
                 (lookup_typ_by_id signature tA_id))
          new_term_subordinations new_type_subordinations
      in
      let newly_frozen_declarations =
        replacements |> Id.Typ.Hamt.keys |> Id.Typ.Set.of_list
      in
      sequence_list
        [ update_typs replacements; freeze_typs newly_frozen_declarations ]
        signature signature'

  let freeze_typ_by_id : Id.Typ.t -> mutation =
   fun id signature signature' ->
    freeze_typ (lookup_typ_by_id signature id) signature signature'

  let freeze_typ_by_ids : Id.Typ.t List.t -> mutation =
   fun ids signature signature' ->
    List.fold_left
      (fun signature id -> freeze_typ_by_id id signature signature')
      signature ids

  (** [freeze_comp_typ cA] is the mutation that freezes at least the
      computational-level data type constant declaration having ID [cA] only
      if it is unfrozen. If that declaration is already frozen, then the
      input signature is returned as is. *)
  let freeze_comp_typ : CompTyp.t -> mutation =
   fun cA signature signature' ->
    let cA_id = CompTyp.id cA in
    try
      let cA' = CompTyp.freeze cA in
      sequence_list
        [ update_comp_typs (Id.CompTyp.Hamt.singleton cA_id cA')
        ; freeze_comp_typs (Id.CompTyp.Set.singleton cA_id)
        ]
        signature signature'
    with CompTyp.FrozenCompTyp _ -> identity signature signature'

  let freeze_comp_typ_by_id : Id.CompTyp.t -> mutation =
   fun id signature signature' ->
    freeze_comp_typ (lookup_comp_typ_by_id signature id) signature signature'

  let freeze_comp_typ_by_ids : Id.CompTyp.t List.t -> mutation =
   fun ids signature signature' ->
    List.fold_left
      (fun signature id -> freeze_comp_typ_by_id id signature signature')
      signature ids

  (** [freeze_comp_cotyp cA] is the mutation that freezes at least the
      computational-level codata type constant declaration having ID [cA]
      only if it is unfrozen. If that declaration is already frozen, then the
      input signature is returned as is. *)
  let freeze_comp_cotyp : CompCotyp.t -> mutation =
   fun cA signature signature' ->
    let cA_id = CompCotyp.id cA in
    try
      let cA' = CompCotyp.freeze cA in
      sequence_list
        [ update_comp_cotyps (Id.CompCotyp.Hamt.singleton cA_id cA')
        ; freeze_comp_cotyps (Id.CompCotyp.Set.singleton cA_id)
        ]
        signature signature'
    with CompCotyp.FrozenCompCotyp _ -> identity signature signature'

  let freeze_comp_cotyp_by_id : Id.CompCotyp.t -> mutation =
   fun id signature signature' ->
    freeze_comp_cotyp
      (lookup_comp_cotyp_by_id signature id)
      signature signature'

  let freeze_comp_cotyp_by_ids : Id.CompCotyp.t List.t -> mutation =
   fun ids signature signature' ->
    List.fold_left
      (fun signature id -> freeze_comp_cotyp_by_id id signature signature')
      signature ids

  let freeze_declaration : declaration -> mutation = function
    | `Typ_declaration declaration -> freeze_typ declaration
    | `Comp_typ_declaration declaration -> freeze_comp_typ declaration
    | `Comp_cotyp_declaration declaration -> freeze_comp_cotyp declaration
    | _ -> identity

  (** [freeze_declaration_by_name name] is the mutation that freezes at least
      the declaration having name [name] only if it is unfrozen. If that
      declaration is already frozen, then the input signature is returned as
      is. *)
  let freeze_declaration_by_name : Name.t -> mutation =
   fun name signature signature' ->
    let open Option in
    lookup_name signature name
    $> (fun declaration ->
         freeze_declaration declaration signature signature')
    |> Option.value ~default:signature

  (** [freeze_declaration_by_name_opt name_opt] is the mutation that freezes
      at least the declaration having name [name] only if it is unfrozen and
      [name_opt] is [Some name]. If that declaration is already frozen or
      [name_opt] is [None], then the input signature is returned as is. *)
  let freeze_declaration_by_name_opt : Name.t Option.t -> mutation =
    Option.eliminate (Fun.const identity) freeze_declaration_by_name

  (** [freeze_all_unfrozen_declarations] is the mutation that freezes all
      unfrozen declarations in the signature. *)
  let freeze_all_unfrozen_declarations : mutation =
   fun signature ->
    sequence_list
      [ freeze_typ_by_ids (typs signature |> Id.Typ.Hamt.keys)
      ; freeze_comp_typ_by_ids (comp_typs signature |> Id.CompTyp.Hamt.keys)
      ; freeze_comp_cotyp_by_ids
          (comp_cotyps signature |> Id.CompCotyp.Hamt.keys)
      ]
      signature
end

let add_typ signature tA =
  let tA_id = Typ.id tA in
  try
    ignore @@ lookup_typ_by_id signature tA_id;
    raise @@ BoundTypId (tA_id, signature)
  with UnboundTypId _ ->
    let tA_name = Typ.name tA
    and tA_declaration = `Typ_declaration tA in
    Mutation.(
      apply_list signature
        [ freeze_declaration_by_name tA_name
        ; add_entry tA_declaration
        ; add_binding tA_name tA_declaration
        ; add_typ tA
        ; add_unfrozen_typ_if (Typ.is_unfrozen tA) tA_id
        ])

let add_const signature tM =
  let tM_id = Const.id tM in
  try
    ignore @@ lookup_const_by_id signature tM_id;
    raise @@ BoundConstId (tM_id, signature)
  with UnboundConstId _ ->
    let tK = lookup_typ_by_id signature (Const.kind tM)
    and tM_name = Const.name tM in
    let tK' = Typ.add_constructor tM_name tM_id tK in
    let tM_declaration = `Const_declaration tM in
    Mutation.(
      apply_list signature
        [ update_typ tK'
        ; freeze_declaration_by_name tM_name
        ; add_entry tM_declaration
        ; add_binding tM_name tM_declaration
        ; add_const tM
        ])

let add_comp_typ signature cA =
  let cA_id = CompTyp.id cA in
  try
    ignore @@ lookup_comp_typ_by_id signature cA_id;
    raise @@ BoundCompTypId (cA_id, signature)
  with UnboundCompTypId _ ->
    let cA_name = CompTyp.name cA
    and cA_declaration = `Comp_typ_declaration cA in
    Mutation.(
      apply_list signature
        [ freeze_declaration_by_name cA_name
        ; add_entry cA_declaration
        ; add_binding cA_name cA_declaration
        ; add_comp_typ cA
        ; add_unfrozen_comp_typ_if (CompTyp.is_unfrozen cA) cA_id
        ])

let add_comp_const signature cM =
  let cM_id = CompConst.id cM in
  try
    ignore @@ lookup_comp_const_by_id signature cM_id;
    raise @@ BoundCompConstId (cM_id, signature)
  with UnboundCompConstId _ ->
    let cK = lookup_comp_typ_by_id signature (CompConst.kind cM)
    and cM_name = CompConst.name cM in
    let cK' = CompTyp.add_constructor cM_name cM_id cK in
    let cM_declaration = `Comp_const_declaration cM in
    Mutation.(
      apply_list signature
        [ update_comp_typ cK'
        ; freeze_declaration_by_name cM_name
        ; add_entry cM_declaration
        ; add_binding cM_name cM_declaration
        ; add_comp_const cM
        ])

let add_comp_cotyp signature cA =
  let cA_id = CompCotyp.id cA in
  try
    ignore @@ lookup_comp_cotyp_by_id signature cA_id;
    raise @@ BoundCompCotypId (cA_id, signature)
  with UnboundCompCotypId _ ->
    let cA_name = CompCotyp.name cA
    and cA_declaration = `Comp_cotyp_declaration cA in
    Mutation.(
      apply_list signature
        [ freeze_declaration_by_name cA_name
        ; add_entry cA_declaration
        ; add_binding cA_name cA_declaration
        ; add_comp_cotyp cA
        ; add_unfrozen_comp_cotyp_if (CompCotyp.is_unfrozen cA) cA_id
        ])

let add_comp_dest signature cM =
  let cM_id = CompDest.id cM in
  try
    ignore @@ lookup_comp_dest_by_id signature cM_id;
    raise @@ BoundCompDestId (cM_id, signature)
  with UnboundCompDestId _ ->
    let cK = lookup_comp_cotyp_by_id signature (CompDest.kind cM)
    and cM_name = CompDest.name cM in
    let cK' = CompCotyp.add_destructor cM_name cM_id cK in
    let cM_declaration = `Comp_dest_declaration cM in
    Mutation.(
      apply_list signature
        [ update_comp_cotyp cK'
        ; freeze_declaration_by_name cM_name
        ; add_entry cM_declaration
        ; add_binding cM_name cM_declaration
        ; add_comp_dest cM
        ])

let add_query signature query =
  let query_id = Query.id query in
  try
    ignore @@ lookup_query_by_id signature query_id;
    raise @@ BoundQueryId (query_id, signature)
  with UnboundQueryId _ ->
    let query_name = Query.name query
    and query_declaration = `Query_declaration query in
    Mutation.(
      apply_list signature
        [ freeze_all_unfrozen_declarations
        ; add_entry query_declaration
        ; add_binding_opt query_name query_declaration
        ; add_query query
        ])

let add_mquery signature mquery =
  let mquery_id = MQuery.id mquery in
  try
    ignore @@ lookup_mquery_by_id signature mquery_id;
    raise @@ BoundMQueryId (mquery_id, signature)
  with UnboundMQueryId _ ->
    let mquery_name = MQuery.name mquery
    and mquery_declaration = `MQuery_declaration mquery in
    Mutation.(
      apply_list signature
        [ freeze_all_unfrozen_declarations
        ; add_entry mquery_declaration
        ; add_binding_opt mquery_name mquery_declaration
        ; add_mquery mquery
        ])

let add_name_pragma signature pragma =
  let tA_id = NamePragma.typ pragma in
  let tA = lookup_typ_by_id signature tA_id in
  let tA' =
    Typ.set_naming_conventions
      ~var:(NamePragma.var_naming_convention pragma)
      ~mvar:(Option.some @@ NamePragma.mvar_naming_convention pragma)
      tA
  in
  Mutation.(apply signature @@ update_typ tA')

let add_documentation_comment signature comment =
  Mutation.(apply signature @@ add_entry (`Documentation_comment comment))

let empty =
  { entries = []
  ; bindings = Name.Hamt.empty
  ; typs = Id.Typ.Hamt.empty
  ; consts = Id.Comp.Hamt.empty
  ; comp_typs = Id.CompTyp.Hamt.empty
  ; comp_consts = Id.CompConst.Hamt.empty
  ; comp_cotyps = Id.CompCotyp.Hamt.empty
  ; comp_dests = Id.CompDest.Hamt.empty
  ; comps = Id.Comp.Hamt.empty
  ; modules = Id.Module.Hamt.empty
  ; schemas = Id.Schema.Hamt.empty
  ; queries = Id.Query.Hamt.empty
  ; mqueries = Id.MQuery.Hamt.empty
  ; unfrozen_typs = Id.Typ.Set.empty
  ; unfrozen_comp_typs = Id.CompTyp.Set.empty
  ; unfrozen_comp_cotyps = Id.CompCotyp.Set.empty
  }

let find_all_queries signature =
  Id.Query.Hamt.values (queries signature) |> List.map (Pair.lmap Lazy.force)

let find_all_mqueries signature =
  Id.MQuery.Hamt.values (mqueries signature)
  |> List.map (Pair.lmap Lazy.force)
