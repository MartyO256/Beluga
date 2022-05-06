(** Beluga signatures.

    @author Marc-Antoine Ouimet *)

open Support
open Id

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
      (base ^ string_of_int i)
      (if Int.equal i Int.max_int then Seq.empty
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

  module Ord' : Ord.ORD with type t = t = Ord.Make (struct
    type nonrec t = t

    module ModuleListOrd : Ord.ORD with type t = Name.t list =
      List.MakeOrd (Name)

    let compare x y =
      let comparison = ModuleListOrd.compare (modules x) (modules y) in
      if Stdlib.(comparison <> 0) then comparison
      else Name.compare (name x) (name y)
  end)

  include (Ord' : Ord.ORD with type t := t)

  module Set = Set.Make (Ord')
  module Map = Map.Make (Ord')
end

(** Unique identifiers for declarations in a signature.

    An ID uniquely refers to a signature declaration in a source file.
    However, since declarations may be elaborated in steps, derived
    declarations share the same ID.

    IDs are generated when initial entries are introduced to a signature. *)
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

  module Ord : Ord.ORD with type t = t = Ord.Make (Int)

  include Ord

  module Hash : Hash.HASH with type t = t = struct
    type nonrec t = t

    let hash x = x
  end

  include (Hash : Support.Hash.HASH with type t := t)

  module Set = Set.Make (Ord)
  module Map = Map.Make (Ord)

  module Hamt = Hamt.Make (struct
    type nonrec t = t

    include (Ord : Support.Ord.ORD with type t := t)

    include (Hash : Support.Hash.HASH with type t := t)
  end)

  let min_value = 0

  let max_value = Int.max_int

  let[@inline] next x = x + 1
end

module Id = struct
  module Typ = BaseId
  module Const = BaseId
  module CompTyp = BaseId
  module CompCotyp = BaseId
  module CompConst = BaseId
  module CompDest = BaseId
  module Comp = BaseId
  module Module = BaseId
  module Query = BaseId
  module MQuery = BaseId
  module Schema = BaseId

  type t =
    [ `Typ_id of Typ.t
    | `Const_id of Const.t
    | `Comp_typ_id of CompTyp.t
    | `Comp_cotyp_id of CompCotyp.t
    | `Comp_const_id of CompConst.t
    | `Comp_dest_id of CompDest.t
    | `Comp_id of Comp.t
    | `Module_id of Module.t
    | `Query_id of Query.t
    | `MQuery_id of MQuery.t
    | `Schema_id of Schema.t
    ]

  let lift_typ_id id = `Typ_id id

  let lift_const_id id = `Const_id id

  let lift_comp_typ_id id = `Comp_typ_id id

  let lift_comp_const_id id = `Comp_const_id id

  let lift_comp_cotyp_id id = `Comp_cotyp_id id

  let lift_comp_dest_id id = `Comp_dest_id id

  let lift_comp_id id = `Comp_id id

  let lift_module_id id = `Module_id id

  let lift_query_id id = `Query_id id

  let lift_mquery_id id = `MQuery_id id

  let lift_schema_id id = `Schema_id id

  let to_base_id : t -> BaseId.t = function
    | `Typ_id id
    | `Const_id id
    | `Comp_typ_id id
    | `Comp_cotyp_id id
    | `Comp_const_id id
    | `Comp_dest_id id
    | `Comp_id id
    | `Module_id id
    | `Query_id id
    | `MQuery_id id
    | `Schema_id id -> id

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
    type id = t

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

(** Beluga Signature Entries *)

module Typ = struct
  open Syntax.Int

  module Unfrozen = struct
    type t =
      { id : Id.Typ.t
      ; name : Name.t
      ; location : Location.t
      ; implicit_arguments : int
      ; kind : LF.kind
      ; var_name_base : Name.t Option.t
      ; mvar_name_base : Name.t Option.t
      ; constructors : Id.Const.t Name.Hamt.t
      }

    let make ~id ~name ~location ~implicit_arguments
        ?(var_name_base = Option.none) ?(mvar_name_base = Option.none)
        ?(constructors = Name.Hamt.empty) kind =
      { id
      ; name
      ; location
      ; implicit_arguments
      ; kind
      ; var_name_base
      ; mvar_name_base
      ; constructors
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
      ; implicit_arguments : int
      ; kind : LF.kind
      ; var_name_base : Name.t Option.t
      ; mvar_name_base : Name.t Option.t
      ; constructors : Id.Const.t Name.Hamt.t
      ; subordinates : Id.Typ.Set.t
      ; type_subordinated : Id.Typ.Set.t
      }

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

  let[@inline] var_name_base = function
    | Frozen { Frozen.var_name_base; _ }
    | Unfrozen { Unfrozen.var_name_base; _ } -> var_name_base

  let[@inline] mvar_name_base = function
    | Frozen { Frozen.mvar_name_base; _ }
    | Unfrozen { Unfrozen.mvar_name_base; _ } -> mvar_name_base

  let[@inline] constructors = function
    | Frozen { Frozen.constructors; _ }
    | Unfrozen { Unfrozen.constructors; _ } -> constructors

  let make_initial_entry ~id ~name ~location ~implicit_arguments kind =
    Unfrozen (Unfrozen.make ~id ~name ~location ~implicit_arguments kind)

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

  let frozen ~subordinates ~type_subordinated
      { Unfrozen.id
      ; name
      ; location
      ; implicit_arguments
      ; kind
      ; var_name_base
      ; mvar_name_base
      ; constructors
      } =
    { Frozen.id
    ; name
    ; location
    ; implicit_arguments
    ; kind
    ; constructors
    ; var_name_base
    ; mvar_name_base
    ; subordinates
    ; type_subordinated
    }

  let freeze ~subordinates ~type_subordinated =
    if_unfrozen (fun x -> Frozen (frozen ~subordinates ~type_subordinated x))

  let fresh_var_name entry ?(default_base_name = "x") =
    entry |> var_name_base |> Option.map Name.show
    |> Option.get_default default_base_name
    |> Name.prefixed_fresh_name_supplier

  let fresh_mvar_name entry ?(default_base_name = "X") =
    entry |> mvar_name_base |> Option.map Name.show
    |> Option.get_default default_base_name
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

  let is_subordinate entry typ =
    entry
    |> if_frozen (fun { Frozen.subordinates; _ } ->
           Id.Typ.Set.mem typ subordinates)

  let is_type_subordinated entry typ =
    entry
    |> if_frozen (fun { Frozen.type_subordinated; _ } ->
           Id.Typ.Set.mem typ type_subordinated)
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
    }

  let make ~id ~name ~location ~implicit_arguments ~kind typ =
    { id; name; location; implicit_arguments; typ; kind }

  let[@inline] id { id; _ } = id

  let[@inline] location { location; _ } = location

  let[@inline] name { name; _ } = name

  let[@inline] typ { typ; _ } = typ

  let[@inline] kind { kind; _ } = kind
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
      }

    let make ~id ~name ~location ~implicit_arguments ~positivity
        ?(constructors = Name.Hamt.empty) kind =
      { id
      ; name
      ; location
      ; implicit_arguments
      ; kind
      ; positivity
      ; constructors
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

  let make_initial_entry ~id ~name ~location ~implicit_arguments ~positivity
      kind =
    Unfrozen
      (Unfrozen.make ~id ~name ~location ~implicit_arguments ~positivity kind)

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
      } =
    { Frozen.id
    ; name
    ; location
    ; implicit_arguments
    ; kind
    ; positivity
    ; constructors
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
    }

  let make ~id ~name ~location ~implicit_arguments ~kind typ =
    { id; name; location; implicit_arguments; typ; kind }

  let[@inline] id { id; _ } = id

  let[@inline] location { location; _ } = location

  let[@inline] name { name; _ } = name

  let[@inline] implicit_arguments { implicit_arguments; _ } =
    implicit_arguments

  let[@inline] typ { typ; _ } = typ

  let[@inline] kind { kind; _ } = kind
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
      }

    let make ~id ~name ~location ~implicit_arguments
        ?(destructors = Name.Hamt.empty) kind =
      { id; name; location; implicit_arguments; kind; destructors }

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

  let make_initial_entry ~id ~name ~location ~implicit_arguments kind =
    Unfrozen (Unfrozen.make ~id ~name ~location ~implicit_arguments kind)

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
      { Unfrozen.id; name; location; implicit_arguments; kind; destructors }
      =
    { Frozen.id; name; location; implicit_arguments; kind; destructors }

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
    }

  let make ~id ~name ~location ~implicit_arguments ~mctx ~observation_typ
      ~return_typ ~kind =
    { id
    ; name
    ; location
    ; implicit_arguments
    ; mctx
    ; observation_typ
    ; return_typ
    ; kind
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
end

module Comp = struct
  open Syntax.Int

  type t =
    { id : Id.Comp.t
    ; name : Name.t
    ; location : Location.t
    ; implicit_arguments : int
    ; typ : Comp.typ
    ; mutual_group : Id.Comp.t Nonempty.t Option.t
    ; program : Comp.value
    }

  let make ~id ~name ~location ~implicit_arguments ~typ
      ?(mutual_group = None) program =
    { id; name; location; implicit_arguments; typ; mutual_group; program }

  let[@inline] id { id; _ } = id

  let[@inline] location { location; _ } = location

  let[@inline] name { name; _ } = name

  let[@inline] implicit_arguments { implicit_arguments; _ } =
    implicit_arguments

  let[@inline] typ { typ; _ } = typ

  let[@inline] program { program; _ } = program

  let[@inline] mutual_group { mutual_group; _ } = mutual_group
end

module Module = struct
  type 'a t =
    { id : Id.Module.t
    ; name : Name.t
    ; location : Location.t
    ; declarations : 'a List.t
    ; declarations_by_name : 'a Nonempty.t Name.Hamt.t
    }

  let make_empty ~id ~location name =
    { id
    ; name
    ; location
    ; declarations = []
    ; declarations_by_name = Name.Hamt.empty
    }

  let add_declaration ({ declarations; _ } as m) declaration =
    { m with declarations = List.cons declaration declarations }

  let add_named_declaration ({ declarations; declarations_by_name; _ } as m)
      name declaration =
    { m with
      declarations = declarations |> List.cons declaration
    ; declarations_by_name =
        declarations_by_name
        |> Name.Hamt.alter name (fun bindings ->
               bindings
               |> Option.eliminate
                    (fun () -> Nonempty.from declaration [])
                    (fun declarations ->
                      Nonempty.cons declaration declarations)
               |> Option.some)
    }

  let[@inline] id { id; _ } = id

  let[@inline] location { location; _ } = location

  let[@inline] name { name; _ } = name

  let[@inline] declarations { declarations; _ } = declarations

  let[@inline] declarations_by_name { declarations_by_name; _ } =
    declarations_by_name

  let lookup m name =
    let open Option in
    m |> declarations_by_name |> Name.Hamt.find_opt name $> Nonempty.head

  let rec deep_lookup :
      ('a -> 'a t Option.t) -> 'a t -> Name.t List.t -> Name.t -> 'a Option.t
      =
   fun extract current_module module_names base_name ->
    match module_names with
    | [] -> lookup current_module base_name
    | head_module_name :: tail_module_names ->
      let open Option in
      lookup current_module head_module_name >>= extract >>= fun m' ->
      deep_lookup extract m' tail_module_names base_name
end

module Schema = struct
  open Syntax.Int

  type t =
    { id : Id.Schema.t
    ; name : Name.t
    ; location : Location.t
    ; schema : LF.schema
    }

  let make ~id ~name ~location schema = { id; name; location; schema }

  let[@inline] id { id; _ } = id

  let[@inline] name { name; _ } = name

  let[@inline] location { location; _ } = location

  let[@inline] schema { schema; _ } = schema
end

module DocumentationComment = struct
  type t =
    { content : string
    ; location : Location.t
    }

  let make ~location content = { content; location }

  let[@inline] content { content; _ } = content

  let[@inline] location { location; _ } = location
end

module Query = struct
  open Syntax.Int

  type search_parameters =
    { expected_solutions : int Option.t
    ; maximum_tries : int Option.t
    ; search_depth : int Option.t
    }

  let make_search_parameters ?(expected_solutions = Option.none)
      ?(maximum_tries = Option.none) ?(search_depth = Option.none) () =
    { expected_solutions; maximum_tries; search_depth }

  type t =
    { id : Id.Query.t
    ; location : Location.t
    ; name : Name.t Option.t
    ; query : LF.mctx * (LF.typ * offset)
    ; search_parameters : search_parameters
    }

  let make ~id ~location ?(name = Option.none)
      ?(search_parameters = make_search_parameters ()) query =
    { id; location; name; search_parameters; query }

  let[@inline] id { id; _ } = id

  let[@inline] location { location; _ } = location

  let[@inline] name { name; _ } = name

  let[@inline] query { query; _ } = query

  let[@inline] search_parameters { search_parameters; _ } = search_parameters
end

module MQuery = struct
  open Syntax.Int

  type search_parameters =
    { expected_solutions : int Option.t
    ; search_tries : int Option.t
    ; search_depth : int Option.t
    ; split_index : int Option.t
    }

  let make_search_parameters ?(expected_solutions = Option.none)
      ?(search_tries = Option.none) ?(search_depth = Option.none)
      ?(split_index = Option.none) () =
    { expected_solutions; search_tries; search_depth; split_index }

  type t =
    { id : Id.MQuery.t
    ; location : Location.t
    ; name : Name.t Option.t
    ; query : Comp.typ * offset
    ; search_parameters : search_parameters
    }

  let make ~id ~location ?(name = Option.none)
      ?(search_parameters = make_search_parameters ()) query =
    { id; location; name; search_parameters; query }

  let[@inline] id { id; _ } = id

  let[@inline] location { location; _ } = location

  let[@inline] name { name; _ } = name

  let[@inline] query { query; _ } = query

  let[@inline] search_parameters { search_parameters; _ } = search_parameters
end

(** Beluga Signatures *)

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

type declaration =
  [ `Typ_declaration of Typ.t
  | `Const_declaration of Const.t
  | `Comp_typ_declaration of CompTyp.t
  | `Comp_const_declaration of CompConst.t
  | `Comp_cotyp_declaration of CompCotyp.t
  | `Comp_dest_declaration of CompDest.t
  | `Comp_declaration of Comp.t
  | `Schema_declaration of Schema.t
  | `Module_declaration of (t * declaration) Module.t
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
  | `Module_declaration of (t * declaration) Module.t
  | `Query_declaration of Query.t
  | `MQuery_declaration of MQuery.t
  ]

and t =
  { declarations : (t * declaration) List.t Lazy.t
        (** The sequence of declarations as they appear in the signature.
            Each entry is also associated with the signature up to and
            including that entry. This allows for in-order traversal of the
            signature for pretty-printing. *)
  ; declarations_by_name : (t * declaration) Nonempty.t Name.Hamt.t Lazy.t
        (** The bindings of entries by name. For a given name, only the head
            element is currently in scope. Each entry is also associated with
            the signature up to and including that entry. This allows for
            entry lookups that respects scoping. *)
  ; declarations_by_id : (t * declaration_with_id) Id.Hamt.t Lazy.t
        (** An index of the entries mapped by ID. Each entry is also
            associated with the signature up to and including that entry.
            This allows for looking up shadowed declarations. *)
  ; paths : QualifiedName.Set.t Id.Hamt.t
        (** The set of qualified names in the signature mapped by declaration
            ID. This allows for determining whether and how an entry is in
            scope in the presence of aliases. These paths are only added to,
            so they may have been shadowed. The mappings of declarations by
            name take precedence over this map to determine declaration
            scoping. *)
  ; queries : Id.Query.Set.t
        (** The set of logic programming queries on LF types. *)
  ; mqueries : Id.MQuery.Set.t
        (** The set of logic programming queries on Comp types. *)
  ; unfrozen : Id.Set.t
        (** The set of entry IDs for currently unfrozen entries. This allows
            for keeping track of entries to freeze before programs. *)
  }

(** Destructors *)

let[@inline] declarations { declarations; _ } = declarations |> Lazy.force

let[@inline] declarations_by_name { declarations_by_name; _ } =
  declarations_by_name |> Lazy.force

let[@inline] declarations_by_id { declarations_by_id; _ } =
  declarations_by_id |> Lazy.force

let[@inline] paths { paths; _ } = paths

let[@inline] unfrozen_entries { unfrozen; _ } = unfrozen

let[@inline] queries { queries; _ } = queries

let[@inline] mqueries { mqueries; _ } = mqueries

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
       [> `Module_declaration of (t * declaration) Module.t ]
    -> (t * declaration) Module.t Option.t = function
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

(** Lookups by Qualified Name *)

let extract_declaration guard (signature, declaration_opt) =
  let open Option in
  declaration_opt |> guard $> Pair.left signature

let lookup_name : t -> Name.t -> (t * declaration) Option.t =
 fun signature name ->
  let open Option in
  signature |> declarations_by_name |> Name.Hamt.find_opt name
  $> Nonempty.head

let lookup signature qualified_name =
  let base_name = QualifiedName.name qualified_name in
  match QualifiedName.modules qualified_name with
  | [] ->
    (* Lookup top-level declaration in signature *)
    lookup_name signature base_name
  | head_module_name :: tail_module_names ->
    (* Lookup recursively in modules *)
    let open Option in
    lookup_name signature head_module_name
    $> Pair.snd >>= guard_module_declaration
    >>= fun top_module ->
    Module.deep_lookup
      (fun looked_up_module ->
        looked_up_module |> Fun.(Pair.snd >> guard_module_declaration))
      top_module tail_module_names base_name

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

(** Lookups by ID *)

(** [lookup signature id] returns [None] if there is no declaration in
    [signature] having ID [id], and otherwise returns
    [Some (signature', declaration)] where [signature'] is the signature up
    to and including [declaration]. Declarations looked up by ID may not be
    in scope. *)
let lookup_by_id signature id =
  signature |> declarations_by_id |> Id.Hamt.find_opt id

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

(** Unsafe Lookups by ID *)

(** [id_of_declaration_with_id declaration] is the lifted ID of
    [declaration]. *)
let id_of_declaration_with_id : [< declaration_with_id ] -> Id.t = function
  | `Typ_declaration declaration -> `Typ_id (declaration |> Typ.id)
  | `Const_declaration declaration -> `Const_id (declaration |> Const.id)
  | `Comp_typ_declaration declaration ->
    `Comp_typ_id (declaration |> CompTyp.id)
  | `Comp_cotyp_declaration declaration ->
    `Comp_cotyp_id (declaration |> CompCotyp.id)
  | `Comp_const_declaration declaration ->
    `Comp_const_id (declaration |> CompConst.id)
  | `Comp_dest_declaration declaration ->
    `Comp_dest_id (declaration |> CompDest.id)
  | `Comp_declaration declaration -> `Comp_id (declaration |> Comp.id)
  | `Module_declaration m -> `Module_id (m |> Module.id)
  | `Query_declaration query -> `Query_id (query |> Query.id)
  | `MQuery_declaration mquery -> `MQuery_id (mquery |> MQuery.id)
  | `Schema_declaration schema -> `Schema_id (schema |> Schema.id)

let id_of_declaration : [< declaration ] -> Id.t Option.t = function
  | #declaration_with_id as declaration ->
    Option.some @@ id_of_declaration_with_id declaration
  | #declaration -> Option.none

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

let is_path_to_entry signature id path =
  let open Option in
  path |> lookup signature >>= fun (signature', declaration) ->
  declaration |> id_of_declaration_exn |> Id.equal id |> Option.of_bool
  $> Fun.const (signature', declaration)

let all_paths_to_entry signature id =
  let open Result in
  signature |> paths |> Id.Hamt.find_opt id
  |> Option.to_result ~none:(`Unbound_id (id, signature))
  $> QualifiedName.Set.filter (fun path ->
         path |> is_path_to_entry signature id |> Option.is_some)

let all_paths_to_entry_exn signature id =
  all_paths_to_entry signature id
  |> Result.get_or_else (fun _ -> raise @@ UnboundId (id, signature))


