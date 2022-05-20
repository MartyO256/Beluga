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

  let[@inline] next x = x + 1
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

  type t =
    | Typ of Typ.t
    | Const of Const.t
    | CompTyp of CompTyp.t
    | CompConst of CompConst.t
    | CompCotyp of CompCotyp.t
    | CompDest of CompDest.t
    | Comp of Comp.t
    | Module of Module.t
    | Query of Query.t
    | MQuery of MQuery.t
    | Schema of Schema.t

  let[@inline] lift_typ_id id = Typ id

  let[@inline] lift_const_id id = Const id

  let[@inline] lift_comp_typ_id id = CompTyp id

  let[@inline] lift_comp_const_id id = CompConst id

  let[@inline] lift_comp_cotyp_id id = CompCotyp id

  let[@inline] lift_comp_dest_id id = CompDest id

  let[@inline] lift_comp_id id = Comp id

  let[@inline] lift_module_id id = Module id

  let[@inline] lift_query_id id = Query id

  let[@inline] lift_mquery_id id = MQuery id

  let[@inline] lift_schema_id id = Schema id

  let to_base_id : t -> BaseId.t = function
    | Typ id
    | Const id
    | CompTyp id
    | CompCotyp id
    | CompConst id
    | CompDest id
    | Comp id
    | Module id
    | Query id
    | MQuery id
    | Schema id -> id

  (* Equality, ordering and hashing are defined by contramapping because IDs
     are allocated using one sequence of integers.

     If a sequence of integers was defined for each ID kind, then equality,
     ordering and hashing would need to consider the ID kind label as
     well. *)

  module OrdByBaseId = (val Ord.contramap (module BaseId) to_base_id)

  include (OrdByBaseId : Ord.ORD with type t := t)

  module HashByBaseId = (val Hash.contramap (module BaseId) to_base_id)

  include (HashByBaseId : Hash.HASH with type t := t)

  module Set = Set.Make (OrdByBaseId)
  module Map = Map.Make (OrdByBaseId)

  module Hamt = Hamt.Make (struct
    include OrdByBaseId
    include HashByBaseId
  end)

  module Allocator = struct
    type state = { previous_id : BaseId.t }

    include (
      State.Make (struct
        type t = state
      end) :
        State.STATE with type state := state)

    let initial_state = { previous_id = BaseId.min_value }

    let next_id =
      get >>= fun { previous_id; _ } ->
      if BaseId.(previous_id = max_value) then
        raise @@ Invalid_argument "Exhausted sequence of fresh IDs"
      else
        let next = BaseId.next previous_id in
        put { previous_id = next } $> Fun.const next

    let next_typ_id = next_id

    let next_const_id = next_id

    let next_comp_typ_id = next_id

    let next_comp_const_id = next_id

    let next_comp_cotyp_id = next_id

    let next_comp_dest_id = next_id

    let next_comp_id = next_id

    let next_module_id = next_id

    let next_query_id = next_id

    let next_mquery_id = next_id

    let next_schema_id = next_id
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

  let lifted_id =
    let id' = id in
    Fun.(id' >> Id.lift_typ_id)

  let make_initial_entry ~id ~name ~location ?documentation_comment kind =
    Unfrozen (Unfrozen.make ~id ~name ~location ~documentation_comment kind)

  let is_frozen = function
    | Frozen _ -> true
    | Unfrozen _ -> false

  let is_unfrozen entry = not @@ is_frozen entry

  let if_unfrozen f = function
    | Frozen { Frozen.id; _ } ->
      Result.error @@ `Frozen_typ_declaration_error id
    | Unfrozen entry -> Result.ok @@ f entry

  let if_frozen f = function
    | Frozen entry -> Result.ok @@ f entry
    | Unfrozen { Unfrozen.id; _ } ->
      Result.error @@ `Unfrozen_typ_declaration_error id

  let has_constructor_with_name name entry =
    entry |> constructors |> Name.Hamt.mem name

  let add_constructor tM_name tM tA =
    let tA_name = name tA in
    let open Result in
    Result.of_bool
      Name.(tA_name <> tM_name)
      (fun () -> `Kind_name_collision (tM_name, tM, tA))
    >>= fun () ->
    Result.of_bool (has_constructor_with_name tM_name tA) (fun () ->
        `Constructor_name_collision (tM_name, tM, tA))
    >>= fun () ->
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

  let lifted_id =
    let id' = id in
    Fun.(id' >> Id.lift_const_id)
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

  let lifted_id =
    let id' = id in
    Fun.(id' >> Id.lift_comp_typ_id)

  let make_initial_entry ~id ~name ~location ~implicit_arguments ~positivity
      ?documentation_comment kind =
    Unfrozen
      (Unfrozen.make ~id ~name ~location ~implicit_arguments ~positivity
         ~documentation_comment kind)

  let is_frozen = function
    | Frozen _ -> true
    | Unfrozen _ -> false

  let is_unfrozen entry = not @@ is_frozen entry

  let if_unfrozen f = function
    | Frozen { Frozen.id; _ } ->
      Result.error @@ `Frozen_comp_typ_declaration_error id
    | Unfrozen entry -> Result.ok @@ f entry

  let if_frozen f = function
    | Frozen entry -> Result.ok @@ f entry
    | Unfrozen { Unfrozen.id; _ } ->
      Result.error @@ `Unfrozen_comp_typ_declaration_error id

  let add_constructor name const =
    if_unfrozen (fun x -> Unfrozen (Unfrozen.add_constructor x name const))

  let has_constructor_with_name name entry =
    entry |> constructors |> Name.Hamt.mem name

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

  let lifted_id =
    let id' = id in
    Fun.(id' >> Id.lift_comp_const_id)
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

  let lifted_id =
    let id' = id in
    Fun.(id' >> Id.lift_comp_cotyp_id)

  let make_initial_entry ~id ~name ~location ~implicit_arguments
      ?documentation_comment kind =
    Unfrozen
      (Unfrozen.make ~id ~name ~location ~implicit_arguments
         ~documentation_comment kind)

  let is_frozen = function
    | Frozen _ -> true
    | Unfrozen _ -> false

  let is_unfrozen entry = not @@ is_frozen entry

  let if_unfrozen f = function
    | Frozen { Frozen.id; _ } ->
      Result.error @@ `Frozen_comp_cotyp_declaration_error id
    | Unfrozen entry -> Result.ok @@ f entry

  let if_frozen f = function
    | Frozen entry -> Result.ok @@ f entry
    | Unfrozen { Unfrozen.id; _ } ->
      Result.error @@ `Unfrozen_comp_cotyp_declaration_error id

  let add_destructor name dest =
    if_unfrozen (fun x -> Unfrozen (Unfrozen.add_destructor x name dest))

  let has_destructor_with_name name entry =
    entry |> destructors |> Name.Hamt.mem name

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

  let lifted_id =
    let id' = id in
    Fun.(id' >> Id.lift_comp_dest_id)
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

  let lifted_id =
    let id' = id in
    Fun.(id' >> Id.lift_comp_id)
end

module Module = struct
  type ('signature, 'declaration, 'declaration_with_id) t =
    { id : Id.Module.t
    ; name : Name.t
    ; location : Location.t
    ; declarations : ('signature * 'declaration) List.t
    ; declarations_by_name :
        ('signature * 'declaration_with_id) List1.t Name.Hamt.t
    ; documentation_comment : DocumentationComment.t Option.t
    }

  let make_empty ~id ~location ?documentation_comment name =
    { id
    ; name
    ; location
    ; declarations = []
    ; declarations_by_name = Name.Hamt.empty
    ; documentation_comment
    }

  let add_declaration ({ declarations; _ } as m) declaration =
    { m with declarations = List.cons declaration declarations }

  let add_to_name_index ({ declarations; declarations_by_name; _ } as m) name
      declaration =
    { m with
      declarations_by_name =
        declarations_by_name
        |> Name.Hamt.alter name (fun bindings ->
               bindings
               |> Option.eliminate
                    (fun () -> List1.from declaration [])
                    (fun declarations -> List1.cons declaration declarations)
               |> Option.some)
    }

  let[@inline] id { id; _ } = id

  let[@inline] location { location; _ } = location

  let[@inline] name { name; _ } = name

  let[@inline] declarations { declarations; _ } = declarations

  let[@inline] declarations_by_name { declarations_by_name; _ } =
    declarations_by_name

  let[@inline] documentation_comment { documentation_comment; _ } =
    documentation_comment

  let lifted_id m = m |> id |> Id.lift_module_id

  let lookup m name =
    let open Option in
    m |> declarations_by_name |> Name.Hamt.find_opt name $> List1.head

  let rec deep_lookup extract current_module module_names base_name =
    match module_names with
    | [] -> lookup current_module base_name
    | head_module_name :: tail_module_names ->
      let open Option in
      lookup current_module head_module_name >>= extract >>= fun m' ->
      deep_lookup extract m' tail_module_names base_name

  let fold_declarations f init m =
    m |> declarations |> List.fold_right (Fun.flip f) |> Fun.apply init
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

  let lifted_id =
    let id' = id in
    Fun.(id' >> Id.lift_schema_id)
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

  let lifted_id =
    let id' = id in
    Fun.(id' >> Id.lift_query_id)
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

  let lifted_id =
    let id' = id in
    Fun.(id' >> Id.lift_mquery_id)
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

and t =
  { declarations : (t * declaration) List.t
        (** The sequence of declarations as they appear in the signature.

            Each declaration is also associated with the signature up to and
            including that declaration. This allows for in-order traversal of
            the signature for pretty-printing.

            This sequence of declarations is only added to. Hence, freezable
            declarations are likely unfrozen at the position in the sequence
            in which they are added. Looking ahead in the sequence of
            declarations is then required to find the first signature that
            contains the declaration as frozen. *)
  ; declarations_by_name : Id.t List1.t Name.Hamt.t
        (** The bindings of entries by name.

            For a given name, only the head element is currently in scope. *)
  ; declarations_by_id : (t * declaration_with_id) Id.Hamt.t
        (** The index of the entries mapped by ID.

            Each declaration is also associated with the signature up to and
            including that declaration. This allows for looking up shadowed
            declarations. *)
  ; paths : QualifiedName.Set.t Id.Hamt.t
        (** The set of qualified names in the signature mapped by declaration
            ID.

            This allows for determining whether and how a declaration is in
            scope in the presence of aliases. These paths are only added to,
            so they may have been shadowed. The mappings of declarations by
            name take precedence over this map to determine declaration
            scoping. *)
  ; queries : Id.Query.Set.t
        (** The set of logic programming queries on LF types. *)
  ; mqueries : Id.MQuery.Set.t
        (** The set of logic programming queries on Comp types. *)
  ; unfrozen_declarations : Id.Set.t
        (** The set of declaration IDs for currently unfrozen declarations.

            This allows for keeping track of declarations to freeze before
            modules, logic programming queries and programs, and at the end
            of signature reconstruction. *)
  }

(** Destructors *)

let[@inline] declarations { declarations; _ } = declarations

let[@inline] declarations_by_name { declarations_by_name; _ } =
  declarations_by_name

let[@inline] declarations_by_id { declarations_by_id; _ } =
  declarations_by_id

let[@inline] paths { paths; _ } = paths

let[@inline] unfrozen_declarations { unfrozen_declarations; _ } =
  unfrozen_declarations

let[@inline] queries { queries; _ } = queries

let[@inline] mqueries { mqueries; _ } = mqueries

(** IDs *)

(** [id_of_declaration_with_id declaration] is the lifted ID of
    [declaration]. *)
let id_of_declaration_with_id : [< declaration_with_id ] -> Id.t = function
  | `Typ_declaration declaration -> Typ.lifted_id declaration
  | `Const_declaration declaration -> Const.lifted_id declaration
  | `Comp_typ_declaration declaration -> CompTyp.lifted_id declaration
  | `Comp_const_declaration declaration -> CompConst.lifted_id declaration
  | `Comp_cotyp_declaration declaration -> CompCotyp.lifted_id declaration
  | `Comp_dest_declaration declaration -> CompDest.lifted_id declaration
  | `Comp_declaration declaration -> Comp.lifted_id declaration
  | `Module_declaration m -> Module.lifted_id m
  | `Query_declaration query -> Query.lifted_id query
  | `MQuery_declaration mquery -> MQuery.lifted_id mquery
  | `Schema_declaration schema -> Schema.lifted_id schema

let id_of_declaration : [< declaration ] -> Id.t Option.t = function
  | #declaration_with_id as declaration ->
    Option.some @@ id_of_declaration_with_id declaration
  | #declaration -> Option.none

(** Mutations *)

(** The type of mutations to a signature.

    Mutations may lazily refer to the signature resulting from applying the
    current mutation. *)
type mutation = t -> t Lazy.t -> t Lazy.t

(** [identity_mutation] performs no mutation on the input signature. *)
let identity_mutation : mutation = fun signature _ -> lazy signature

(** [sequence_mutations mutations] constructs the mutation that performs the
    mutations in [mutations] in order. *)
let sequence_mutations : mutation List.t -> mutation =
 fun mutations signature signature' ->
  lazy
    (List.fold_left
       (fun signature mutation ->
         Lazy.force @@ mutation signature signature')
       signature mutations)

(** [apply_mutation signature mutation] calls [mutation] on [signature] and
    on the mutation result recursively. *)
let apply_mutation : t -> mutation -> t =
 fun signature mutation ->
  let rec signature' = lazy (Lazy.force @@ mutation signature signature') in
  Lazy.force signature'

(** [apply_mutations signature mutations] sequences the mutations [mutations]
    and applies them on [signature]. *)
let apply_mutations : t -> mutation List.t -> t =
 fun signature mutations ->
  sequence_mutations mutations |> apply_mutation signature

(** Simple mutations *)

(** [add_declaration_to_list declaration] is the mutation that adds
    [declaration] to the signature's {!recfield:declarations} field. *)
let add_declaration_to_list : [< declaration ] -> mutation =
 fun new_declaration signature signature' ->
  lazy
    { signature with
      declarations =
        declarations signature
        |> List.cons (Lazy.force signature', new_declaration)
    }

(** [add_declarations_to_list declaration] is the mutation that sequentially
    adds the declarations in [declarations] to the signature's
    {!recfield:declarations} field. *)
let add_declarations_to_list : [< declaration ] List.t -> mutation =
 fun new_declarations signature signature' ->
  lazy
    { signature with
      declarations =
        declarations signature
        |> List.append
             (new_declarations
             |> List.map (Pair.left (Lazy.force signature')))
    }

(** [add_declaration_by_name (name, id)] is the mutation that adds the
    declaration having name [name] and ID [id] to the signature's
    {!recfield:declarations_by_name} field. *)
let add_declaration_by_name : Name.t * Id.t -> mutation =
 fun (name, declaration_id) signature _ ->
  lazy
    { signature with
      declarations_by_name =
        declarations_by_name signature
        |> Name.Hamt.alter name
             (Option.eliminate
                (fun () -> Option.some @@ List1.singleton declaration_id)
                Fun.(List1.cons declaration_id >> Option.some))
    }

(** [add_declaration_by_name declarations] is the mutation that adds the
    declaration IDs [declarations] mapped by name to the signature's
    {!recfield:declarations_by_name} field. *)
let add_declarations_by_name : Id.t Name.Hamt.t -> mutation =
 fun declarations signature signature' ->
  lazy
    (Name.Hamt.fold
       (fun name declaration_id signature ->
         Lazy.force
         @@ add_declaration_by_name (name, declaration_id) signature
              signature')
       declarations signature)

(** [add_declaration_by_id (id, declaration)] is the mutation that adds the
    declaration [declaration] having ID [id] to the signature's
    {!recfield:declarations_by_id} field. *)
let add_declaration_by_id : Id.t * [< declaration_with_id ] -> mutation =
 fun (id, declaration) signature signature' ->
  lazy
    { signature with
      declarations_by_id =
        declarations_by_id signature
        |> Id.Hamt.add id (Lazy.force signature', declaration)
    }

(** [add_declarations_by_id declarations] is the mutation that adds the
    declarations [declarations] mapped by ID to the signature's
    {!recfield:declarations_by_id} field. *)
let add_declarations_by_id : [< declaration_with_id ] Id.Hamt.t -> mutation =
 fun declarations signature signature' ->
  lazy
    (Id.Hamt.fold
       (fun id declaration signature ->
         Lazy.force
         @@ add_declaration_by_id (id, declaration) signature signature')
       declarations signature)

(** [update_declaration declarations] is the mutation that replaces the
    declaration [declaration] in the signature's
    {!recfield:declarations_by_id} field. *)
let update_declaration : [< declaration_with_id ] -> mutation =
 fun declaration signature signature' ->
  lazy
    { signature with
      declarations_by_id =
        declarations_by_id signature
        |> Id.Hamt.alter (id_of_declaration_with_id declaration)
           @@ Fun.const
           @@ Option.some (Lazy.force signature', declaration)
    }

(** [update_declaration_by_id (id, declaration)] is functionally equivalent
    to {!update_declaration} excepth that the ID of [declaration] is not
    looked up. *)
let update_declaration_by_id : Id.t * [< declaration_with_id ] -> mutation =
 fun (id, declaration) signature signature' ->
  lazy
    { signature with
      declarations_by_id =
        declarations_by_id signature
        |> Id.Hamt.alter id @@ Fun.const
           @@ Option.some (Lazy.force signature', declaration)
    }

(** [update_declarations_by_id declarations] is the mutation that replaces
    the declarations [declarations] mapped by ID in the signature's
    {!recfield:declarations_by_id} field. *)
let update_declarations_by_id :
    [< declaration_with_id ] Id.Hamt.t -> mutation =
 fun declarations signature signature' ->
  lazy
    (Id.Hamt.fold
       (fun id declaration signature ->
         Lazy.force
         @@ update_declaration_by_id (id, declaration) signature signature')
       declarations signature)

(** [add_path (id, path)] is the mutation that adds the path [path] to the
    declaration having ID [id] in the signature's {!recfield:paths} field. *)
let add_path : Id.t * QualifiedName.t -> mutation =
 fun (id, path) signature _ ->
  lazy
    { signature with
      paths =
        paths signature
        |> Id.Hamt.alter id
             (Option.eliminate
                (fun () -> Option.some @@ QualifiedName.Set.singleton path)
                Fun.(QualifiedName.Set.add path >> Option.some))
    }

(** [add_query id] is the mutation that adds the query with ID [id] in the
    signature's {!recfield:queries} field. *)
let add_query : Id.Query.t -> mutation =
 fun query_id signature _ ->
  lazy
    { signature with
      queries = queries signature |> Id.Query.Set.add query_id
    }

(** [add_mquery id] is the mutation that adds the meta-query with ID [id] in
    the signature's {!recfield:mqueries} field. *)
let add_mquery : Id.MQuery.t -> mutation =
 fun mquery_id signature _ ->
  lazy
    { signature with
      mqueries = mqueries signature |> Id.MQuery.Set.add mquery_id
    }

(** [add_unfrozen_declaration id] is the mutation that adds the declaration
    with ID [id] in the signature's {!recfield:unfrozen_declarations} field. *)
let add_unfrozen_declaration : Id.t -> mutation =
 fun id signature _ ->
  lazy
    { signature with
      unfrozen_declarations = Id.Set.add id (unfrozen_declarations signature)
    }

(** [add_unfrozen_declaration ids] is the mutation that adds the declarations
    with IDs [ids] in the signature's {!recfield:unfrozen_declarations}
    field. *)
let add_unfrozen_declarations : Id.Set.t -> mutation =
 fun ids signature _ ->
  lazy
    { signature with
      unfrozen_declarations =
        Id.Set.union ids (unfrozen_declarations signature)
    }

(** [add_unfrozen_declaration_if cond id] is the mutation that adds the
    declaration with ID [id] in the signature's
    {!recfield:unfrozen_declarations} field only if [cond = true]. *)
let add_unfrozen_declaration_if : bool -> Id.t -> mutation =
 fun frozen id ->
  if frozen then identity_mutation else add_unfrozen_declaration id

(** [remove_unfrozen_declaration id] is the mutation that removes the
    declaration with ID [id] in the signature's
    {!recfield:unfrozen_declarations} field. *)
let remove_unfrozen_declaration : Id.t -> mutation =
 fun id_to_remove signature _ ->
  lazy
    { signature with
      unfrozen_declarations =
        Id.Set.remove id_to_remove (unfrozen_declarations signature)
    }

(** [remove_unfrozen_declarations ids] is the mutation that removes the
    declarations with IDs [ids] in the signature's
    {!recfield:unfrozen_declarations} field. *)
let remove_unfrozen_declarations : Id.Set.t -> mutation =
 fun ids_to_remove signature _ ->
  lazy
    { signature with
      unfrozen_declarations =
        Id.Set.diff (unfrozen_declarations signature) ids_to_remove
    }

(** Composite Mutations *)

(** [lift_declaration_with_id declaration] is [declaration].

    This is a weird workaround for an expression in {!val:add_declaration}
    where a value of type {!type:declaration_with_id} cannot be used in place
    of type {!type:declaration} despite having that
    [declaration_with_id <: declaration]. *)
let lift_declaration_with_id : declaration_with_id -> declaration = function
  | ( `Typ_declaration _
    | `Const_declaration _
    | `Comp_typ_declaration _
    | `Comp_const_declaration _
    | `Comp_cotyp_declaration _
    | `Comp_dest_declaration _
    | `Comp_declaration _
    | `Module_declaration _
    | `Query_declaration _
    | `MQuery_declaration _
    | `Schema_declaration _ ) as x -> x

(** [add_declaration (id, name, declaration)] is the composite mutation that
    adds

    - [declaration] to the list of declarations,
    - [(id, declaration)] to the index of declarations by id,
    - [(name, id)] to the index of declarations by name, and
    - [(id, QualifiedName.make name)] to the index of paths by declaration
      ID.

    This is an optimization to avoid intermediary memory allocations for
    performing those mutations in sequence. *)
let add_declaration : Id.t * Name.t * [< declaration_with_id ] -> mutation =
 fun (declaration_id, declaration_name, declaration) signature signature' ->
  lazy
    { signature with
      declarations =
        declarations signature
        |> List.cons
             (Lazy.force signature', lift_declaration_with_id declaration)
    ; declarations_by_name =
        declarations_by_name signature
        |> Name.Hamt.alter declaration_name
             (Option.eliminate
                (fun () -> Option.some @@ List1.singleton declaration_id)
                Fun.(List1.cons declaration_id >> Option.some))
    ; declarations_by_id =
        declarations_by_id signature
        |> Id.Hamt.add declaration_id (Lazy.force signature', declaration)
    ; paths =
        (let path = QualifiedName.make declaration_name in
         paths signature
         |> Id.Hamt.alter declaration_id
              (Option.eliminate
                 (fun () -> Option.some @@ QualifiedName.Set.singleton path)
                 Fun.(QualifiedName.Set.add path >> Option.some)))
    }

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
       [> `Module_declaration of
          ('signature, 'declaration, 'declaration_with_id) Module.t
       ]
    -> ('signature, 'declaration, 'declaration_with_id) Module.t Option.t =
  function
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

(** Lookups by ID *)

(** [lookup signature id] returns [None] if there is no declaration in
    [signature] having ID [id], and otherwise returns
    [Some (signature', declaration)] where [signature'] is the signature up
    to and including [declaration]. Declarations looked up by ID may not be
    in scope. *)
let lookup_by_id : t -> Id.t -> (t * declaration_with_id) Option.t =
 fun signature id -> signature |> declarations_by_id |> Id.Hamt.find_opt id

let guarded_lookup_by_id lift_id guard signature id =
  let open Option in
  id |> Fun.(lift_id >> lookup_by_id signature) >>= extract_declaration guard

let lookup_typ_by_id =
  guarded_lookup_by_id Id.lift_typ_id guard_typ_declaration

let lookup_constructor_by_id =
  guarded_lookup_by_id Id.lift_const_id guard_const_declaration

let lookup_comp_typ_by_id =
  guarded_lookup_by_id Id.lift_comp_typ_id guard_comp_typ_declaration

let lookup_comp_constructor_by_id =
  guarded_lookup_by_id Id.lift_comp_const_id guard_comp_const_declaration

let lookup_comp_cotyp_by_id =
  guarded_lookup_by_id Id.lift_comp_cotyp_id guard_comp_cotyp_declaration

let lookup_comp_destructor_by_id =
  guarded_lookup_by_id Id.lift_comp_dest_id guard_comp_dest_declaration

let lookup_comp_by_id =
  guarded_lookup_by_id Id.lift_comp_id guard_comp_declaration

let lookup_schema_by_id =
  guarded_lookup_by_id Id.lift_schema_id guard_schema_declaration

let lookup_module_by_id =
  guarded_lookup_by_id Id.lift_module_id guard_module_declaration

let lookup_query_by_id =
  guarded_lookup_by_id Id.lift_query_id guard_query_declaration

let lookup_mquery_by_id =
  guarded_lookup_by_id Id.lift_mquery_id guard_mquery_declaration

let extract_entry_from_lookup lookup signature id =
  let open Option in
  lookup signature id $> Pair.snd

let lookup_typ_by_id' = extract_entry_from_lookup lookup_typ_by_id

let lookup_constructor_by_id' =
  extract_entry_from_lookup lookup_constructor_by_id

let lookup_comp_typ_by_id' = extract_entry_from_lookup lookup_comp_typ_by_id

let lookup_comp_constructor_by_id' =
  extract_entry_from_lookup lookup_comp_constructor_by_id

let lookup_comp_cotyp_by_id' =
  extract_entry_from_lookup lookup_comp_cotyp_by_id

let lookup_comp_destructor_by_id' =
  extract_entry_from_lookup lookup_comp_destructor_by_id

let lookup_comp_by_id' = extract_entry_from_lookup lookup_comp_by_id

let lookup_schema_by_id' = extract_entry_from_lookup lookup_schema_by_id

let lookup_module_by_id' = extract_entry_from_lookup lookup_module_by_id

let lookup_query_by_id' = extract_entry_from_lookup lookup_query_by_id

let lookup_mquery_by_id' = extract_entry_from_lookup lookup_mquery_by_id

(** Unsafe Lookups by ID *)

exception DeclarationWithoutId of declaration

let id_of_declaration_exn : [< declaration ] -> Id.t = function
  | #declaration_with_id as declaration ->
    id_of_declaration_with_id declaration
  | #declaration as declaration -> raise @@ DeclarationWithoutId declaration

exception UnboundId of Id.t * t

type id_kind_mismatch =
  { bound : Id.t
  ; expected : Id.t
  ; signature : t
  }

exception IdKindMismatch of id_kind_mismatch

let lookup_by_id_exn lift_id guard_declaration signature id =
  let lifted_id = lift_id id in
  lookup_by_id signature lifted_id
  |> Option.get_or_else (fun () -> raise @@ UnboundId (lifted_id, signature))
  |> fun (signature, declaration) ->
  declaration |> guard_declaration
  |> Option.eliminate
       (fun () ->
         raise
         @@ IdKindMismatch
              { bound = id_of_declaration_exn declaration
              ; expected = lifted_id
              ; signature
              })
       (Pair.left signature)

let lookup_typ_by_id_exn =
  lookup_by_id_exn Id.lift_typ_id guard_typ_declaration

let lookup_constructor_by_id_exn =
  lookup_by_id_exn Id.lift_const_id guard_const_declaration

let lookup_comp_typ_by_id_exn =
  lookup_by_id_exn Id.lift_comp_typ_id guard_comp_typ_declaration

let lookup_comp_constructor_by_id_exn =
  lookup_by_id_exn Id.lift_comp_const_id guard_comp_const_declaration

let lookup_comp_cotyp_by_id_exn =
  lookup_by_id_exn Id.lift_comp_cotyp_id guard_comp_cotyp_declaration

let lookup_comp_destructor_by_id_exn =
  lookup_by_id_exn Id.lift_comp_dest_id guard_comp_dest_declaration

let lookup_comp_by_id_exn =
  lookup_by_id_exn Id.lift_comp_id guard_comp_declaration

let lookup_schema_by_id_exn =
  lookup_by_id_exn Id.lift_schema_id guard_schema_declaration

let lookup_module_by_id_exn =
  lookup_by_id_exn Id.lift_module_id guard_module_declaration

let lookup_query_by_id_exn =
  lookup_by_id_exn Id.lift_query_id guard_query_declaration

let lookup_mquery_by_id_exn =
  lookup_by_id_exn Id.lift_mquery_id guard_mquery_declaration

let extract_entry_from_lookup_exn lookup signature id =
  lookup signature id |> Pair.snd

let lookup_typ_by_id_exn' =
  extract_entry_from_lookup_exn lookup_typ_by_id_exn

let lookup_constructor_by_id_exn' =
  extract_entry_from_lookup_exn lookup_constructor_by_id_exn

let lookup_comp_typ_by_id_exn' =
  extract_entry_from_lookup_exn lookup_comp_typ_by_id_exn

let lookup_comp_constructor_by_id_exn' =
  extract_entry_from_lookup_exn lookup_comp_constructor_by_id_exn

let lookup_comp_cotyp_by_id_exn' =
  extract_entry_from_lookup_exn lookup_comp_cotyp_by_id_exn

let lookup_comp_destructor_by_id_exn' =
  extract_entry_from_lookup_exn lookup_comp_destructor_by_id_exn

let lookup_comp_by_id_exn' =
  extract_entry_from_lookup_exn lookup_comp_by_id_exn

let lookup_schema_by_id_exn' =
  extract_entry_from_lookup_exn lookup_schema_by_id_exn

let lookup_module_by_id_exn' =
  extract_entry_from_lookup_exn lookup_module_by_id_exn

let lookup_query_by_id_exn' =
  extract_entry_from_lookup_exn lookup_query_by_id_exn

let lookup_mquery_by_id_exn' =
  extract_entry_from_lookup_exn lookup_mquery_by_id_exn

(** Lookups by Qualified Name *)

let lookup_name : t -> Name.t -> (t * declaration_with_id) Option.t =
 fun signature name ->
  let open Option in
  signature |> declarations_by_name |> Name.Hamt.find_opt name $> List1.head
  >>= lookup_by_id signature

let lookup_name' : t -> Name.t -> declaration_with_id Option.t =
 fun signature name ->
  let open Option in
  lookup_name signature name $> Pair.snd

let lookup signature qualified_name =
  let base_name = QualifiedName.name qualified_name in
  match QualifiedName.modules qualified_name with
  | [] ->
    (* Lookup top-level declaration in signature *)
    lookup_name signature base_name
  | head_module_name :: tail_module_names ->
    (* Lookup recursively in modules *)
    let open Option in
    head_module_name |> lookup_name signature $> Pair.snd
    >>= guard_module_declaration
    >>= fun top_module ->
    Module.deep_lookup
      Fun.(Pair.snd >> guard_module_declaration)
      top_module tail_module_names base_name

let lookup' signature qualified_name =
  let open Option in
  lookup signature qualified_name $> Pair.snd

let guarded_declaration_lookup guard signature qualified_name =
  let open Option in
  lookup signature qualified_name >>= extract_declaration guard

let lookup_typ = guarded_declaration_lookup guard_typ_declaration

let lookup_constructor = guarded_declaration_lookup guard_const_declaration

let lookup_comp_typ = guarded_declaration_lookup guard_comp_typ_declaration

let lookup_comp_constructor =
  guarded_declaration_lookup guard_comp_const_declaration

let lookup_comp_cotyp =
  guarded_declaration_lookup guard_comp_cotyp_declaration

let lookup_comp_destructor =
  guarded_declaration_lookup guard_comp_dest_declaration

let lookup_comp = guarded_declaration_lookup guard_comp_declaration

let lookup_schema = guarded_declaration_lookup guard_schema_declaration

let lookup_module = guarded_declaration_lookup guard_module_declaration

let lookup_query = guarded_declaration_lookup guard_query_declaration

let lookup_mquery = guarded_declaration_lookup guard_mquery_declaration

module Subordination = struct
  open Syntax.Int

  type state =
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
    ; new_term_subordinations : Id.Typ.Set.t Id.Typ.Map.t
          (** The mapping from LF families to their term-level subordinates.

              These are the term-level subordination relations discovered
              after the initial state. To check whether an LF family is a
              term-level subordinate of another LF family using both these
              new subordination relations and the previously known ones, see
              {!lookup_is_subordinate}. *)
    ; new_type_subordinations : Id.Typ.Set.t Id.Typ.Map.t
          (** The mapping from LF families to their type-level subordinates.

              These are the type-level subordination relations discovered
              after the initial state. To check whether an LF family is a
              type-level subordinate of another LF family using both these
              new subordination relations and the previously known ones, see
              {!lookup_is_type_subordinate}. *)
    }

  include (
    State.Make (struct
      type t = state
    end) :
      State.STATE with type state := state)

  let[@inline] lookup_kind { lookup_kind; _ } = lookup_kind

  let[@inline] lookup_constructors { lookup_constructors; _ } =
    lookup_constructors

  let[@inline] is_term_subordinate_known { is_term_subordinate_known; _ } =
    is_term_subordinate_known

  let[@inline] is_type_subordinate_to_known
      { is_type_subordinate_to_known; _ } =
    is_type_subordinate_to_known

  let[@inline] new_term_subordinations { new_term_subordinations; _ } =
    new_term_subordinations

  let[@inline] new_type_subordinations { new_type_subordinations; _ } =
    new_type_subordinations

  let initial_state :
         lookup_kind:(Id.Typ.t -> LF.kind)
      -> lookup_constructors:(Id.Typ.t -> LF.typ list)
      -> is_term_subordinate_known:(Id.Typ.t -> Id.Typ.t -> bool Option.t)
      -> is_type_subordinate_to_known:(Id.Typ.t -> Id.Typ.t -> bool Option.t)
      -> state =
   fun ~lookup_kind ~lookup_constructors ~is_term_subordinate_known
       ~is_type_subordinate_to_known ->
    { lookup_kind
    ; lookup_constructors
    ; is_term_subordinate_known
    ; is_type_subordinate_to_known
    ; new_term_subordinations = Id.Typ.Map.empty
    ; new_type_subordinations = Id.Typ.Map.empty
    }

  let lookup_kind tA = get $> Fun.(lookup_kind >> Fun.apply tA)

  let lookup_constructors tA =
    get $> Fun.(lookup_constructors >> Fun.apply tA)

  let lookup_old_term_subordinations tA tB =
    get $> Fun.(is_term_subordinate_known >> Fun.apply tA >> Fun.apply tB)

  let lookup_old_type_subordinations tA tB =
    get $> Fun.(is_type_subordinate_to_known >> Fun.apply tA >> Fun.apply tB)

  let lookup_new_term_subordinations tA =
    get $> Fun.(new_term_subordinations >> Id.Typ.Map.find_opt tA)

  let lookup_new_type_subordinations tA =
    get $> Fun.(new_type_subordinations >> Id.Typ.Map.find_opt tA)

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

  type subordinations =
    { term_subordinates : Id.Typ.Set.t Id.Typ.Map.t
          (** The mapping from LF families to their term-level subordinates. *)
    ; type_subordinated_to : Id.Typ.Set.t Id.Typ.Map.t
          (** The mapping from LF families to the LF families they are
              subordinate to. *)
    }

  let compute_subordinations : Id.Typ.t -> state -> subordinations =
    let compute_subordinations : Id.Typ.t -> subordinations t =
     fun _ _ -> Error.not_implemented' "[compute_subordinations tA state]"
    in
    fun tA state -> run ~init:state (compute_subordinations tA) |> Pair.snd
end

let empty_subordination_state : t -> Subordination.state =
 fun signature ->
  let lookup_kind = Fun.(lookup_typ_by_id_exn' signature >> Typ.kind) in
  let lookup_constructors =
    Fun.(
      lookup_typ_by_id_exn' signature
      >> Typ.constructors >> Name.Hamt.values
      >> List.map (lookup_constructor_by_id_exn' signature >> Const.typ))
  in
  let is_term_subordinate_known =
    Fun.(lookup_typ_by_id_exn' signature >> Typ.is_term_subordinate)
  in
  let is_term_subordinate_known_opt tA tB =
    is_term_subordinate_known tA tB |> Result.to_option
  in
  let is_type_subordinate_to_known =
    Fun.(lookup_typ_by_id_exn' signature >> Typ.is_type_subordinate_to)
  in
  let is_type_subordinate_to_known_opt tA tB =
    is_type_subordinate_to_known tA tB |> Result.to_option
  in
  Subordination.initial_state ~lookup_kind ~lookup_constructors
    ~is_term_subordinate_known:is_term_subordinate_known_opt
    ~is_type_subordinate_to_known:is_type_subordinate_to_known_opt

let is_path_to_entry signature id path =
  let open Option in
  path |> lookup signature >>= fun (signature', declaration) ->
  declaration |> id_of_declaration_with_id |> Id.equal id |> Option.of_bool
  $> Fun.const (signature', declaration)

let all_paths_to_entry signature id' =
  let open Result in
  signature |> paths |> Id.Hamt.find_opt id'
  |> Option.to_result ~none:(`Unbound_id (id', signature))
  $> QualifiedName.Set.filter
       Fun.(is_path_to_entry signature id' >> Option.is_some)

let all_paths_to_entry_exn signature id =
  all_paths_to_entry signature id
  |> Result.get_or_else (fun _ -> raise @@ UnboundId (id, signature))

let guard_unbound_id signature id unbound_continuation =
  lookup_by_id signature id
  |> Option.eliminate unbound_continuation (fun declaration ->
         Result.error @@ `Bound_id (id, declaration, signature))

let guard_bound_id signature id bound_continuation =
  lookup_by_id signature id
  |> Option.eliminate
       (fun () -> Result.error @@ `Unbound_id (id, signature))
       bound_continuation

let is_declaration_unfrozen_by_id : Id.t -> t -> bool =
 fun id signature -> unfrozen_declarations signature |> Id.Set.mem id

let is_declaration_frozen_by_id : Id.t -> t -> bool =
 fun id signature -> Bool.not @@ is_declaration_unfrozen_by_id id signature

let freeze_typ_declaration : Id.Typ.t -> mutation =
 fun tA_id signature signature' ->
  if Typ.is_frozen @@ lookup_typ_by_id_exn' signature tA_id then
    lazy signature
  else
    let { Subordination.term_subordinates; type_subordinated_to } =
      Subordination.compute_subordinations tA_id
        (empty_subordination_state signature)
    in
    let replacements =
      Id.Typ.Map.merge
        (fun tA_id term_subordinates type_subordinated_to ->
          tA_id
          |> lookup_typ_by_id_exn' signature
          |> Typ.freeze
               ~term_subordinates:
                 (Option.value ~default:Id.Typ.Set.empty term_subordinates)
               ~type_subordinated_to:
                 (Option.value ~default:Id.Typ.Set.empty type_subordinated_to)
          |> Result.to_option)
        term_subordinates type_subordinated_to
      |> Id.Typ.Map.fold (fun key entry ->
             Id.Hamt.add (Id.lift_typ_id key) (`Typ_declaration entry))
      |> Fun.apply Id.Hamt.empty
    in
    let newly_frozen_declarations =
      replacements |> Id.Hamt.keys |> Id.Set.of_list
    in
    sequence_mutations
      [ update_declarations_by_id replacements
      ; remove_unfrozen_declarations newly_frozen_declarations
      ]
      signature signature'

let freeze_comp_typ_declaration : Id.CompTyp.t -> mutation =
 fun id signature signature' ->
  id
  |> lookup_comp_typ_by_id_exn' signature
  |> CompTyp.freeze
  |> Result.fold
       ~error:(Fun.const (lazy signature))
       ~ok:(fun cA ->
         let cA_id = CompTyp.lifted_id cA in
         sequence_mutations
           [ update_declaration_by_id (cA_id, `Comp_typ_declaration cA)
           ; remove_unfrozen_declaration cA_id
           ]
           signature signature')

let freeze_comp_cotyp_declaration : Id.CompCotyp.t -> mutation =
 fun id signature signature' ->
  id
  |> lookup_comp_cotyp_by_id_exn' signature
  |> CompCotyp.freeze
  |> Result.fold
       ~error:(Fun.const (lazy signature))
       ~ok:(fun cA ->
         let cA_id = CompCotyp.lifted_id cA in
         sequence_mutations
           [ update_declaration_by_id (cA_id, `Comp_cotyp_declaration cA)
           ; remove_unfrozen_declaration cA_id
           ]
           signature signature')

let freeze_declaration_by_id : Id.t -> mutation =
 fun id signature signature' ->
  match id with
  | Id.Typ id -> freeze_typ_declaration id signature signature'
  | Id.CompTyp id -> freeze_comp_typ_declaration id signature signature'
  | Id.CompCotyp id -> freeze_comp_cotyp_declaration id signature signature'
  | _ -> lazy signature

let freeze_declaration_by_name : Name.t -> mutation =
 fun name signature signature' ->
  let open Option in
  lookup_name' signature name
  $> (fun declaration ->
       freeze_declaration_by_id
         (id_of_declaration_with_id declaration)
         signature signature')
  |> Option.value ~default:(lazy signature)

let add_typ signature tA =
  let tA_id = tA |> Typ.id |> Id.lift_typ_id in
  guard_unbound_id signature tA_id (fun () ->
      let tA_name = Typ.name tA
      and tA_declaration = `Typ_declaration tA in
      Result.ok
      @@ apply_mutations signature
           [ freeze_declaration_by_name tA_name
           ; add_declaration (tA_id, tA_name, tA_declaration)
           ; add_unfrozen_declaration_if (Typ.is_unfrozen tA) tA_id
           ])

let add_const signature tM =
  let tM_id = Const.lifted_id tM in
  guard_unbound_id signature tM_id (fun () ->
      let tK_id = Const.kind tM in
      let open Result in
      tK_id
      |> lookup_typ_by_id' signature
      |> Option.to_result ~none:(`Unbound_typ_id tK_id)
      >>= fun tK ->
      let tM_name = Const.name tM in
      Typ.add_constructor tM_name (Const.id tM) tK $> fun tK ->
      let tM_declaration = `Const_declaration tM in
      apply_mutations signature
        [ update_declaration (`Typ_declaration tK)
        ; freeze_declaration_by_name tM_name
        ; add_declaration (tM_id, tM_name, tM_declaration)
        ])

let empty =
  { declarations = []
  ; declarations_by_name = Name.Hamt.empty
  ; declarations_by_id = Id.Hamt.empty
  ; paths = Id.Hamt.empty
  ; queries = Id.Query.Set.empty
  ; mqueries = Id.MQuery.Set.empty
  ; unfrozen_declarations = Id.Set.empty
  }
