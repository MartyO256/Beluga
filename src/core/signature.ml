(** Beluga signatures.

    @author Marc-Antoine Ouimet *)

open Support
open Id

module Name = struct
  type t = string

  include (Eq.Make (String) : Eq.EQ with type t := t)

  include (Ord.Make (String) : Ord.ORD with type t := t)

  include (
    struct
      let pp ppf name = Format.fprintf ppf "%s" name

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
  type name = Name.t

  type t =
    { name : name
    ; modules : name List.t
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
        List.equal Name.equal (modules x) (modules y)
        && Name.equal (name x) (name y)
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

module Declaration = struct
  type name = Name.t

  type 'entry t =
    { name : name
    ; entry : 'entry
    }

  let[@inline] make ~name ~entry = { name; entry }

  let[@inline] name { name; _ } = name

  let[@inline] entry { entry; _ } = entry
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

  (** {1 Collections} *)

  module Set : Set.S with type elt := t

  module Map : Map.S with type key := t
end

(** Base implementation for IDs as integers. *)
module BaseId : sig
  (** Unique identifiers for declarations in a signature as integers.

      This module type enables internal modules to construct IDs as integers.
      The type for IDs remains abstract in exported module signatures. *)
  include ID with type t = int

  (** {1 Constructors} *)

  val make : int -> t
end = struct
  include Int
  include Ord.Make (Int)
  module Set = Set.Make (Int)
  module Map = Map.Make (Int)

  let make = Fun.id
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
  module DocumentationComment = BaseId
  module Query = BaseId
  module MQuery = BaseId
  module Schema = BaseId
end

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
      ; constructors : Id.Const.t Name.Map.t
      }

    let make ~id ~name ~location ~implicit_arguments
        ?(var_name_base = Option.none) ?(mvar_name_base = Option.none)
        ?(constructors = Name.Map.empty) kind =
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
      { entry with constructors = Name.Map.add name const constructors }

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
      ; constructors : Id.Const.t Name.Map.t
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

  let add_constructor name const =
    if_unfrozen (fun x -> Unfrozen (Unfrozen.add_constructor x name const))

  let has_constructor_with_name name entry =
    entry |> constructors |> Name.Map.mem name

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
      ; constructors : Id.CompConst.t Name.Map.t
      }

    let make ~id ~name ~location ~implicit_arguments ~positivity
        ?(constructors = Name.Map.empty) kind =
      { id
      ; name
      ; location
      ; implicit_arguments
      ; kind
      ; positivity
      ; constructors
      }

    let add_constructor ({ constructors; _ } as entry) name const =
      { entry with constructors = Name.Map.add name const constructors }
  end

  module Frozen = struct
    type t =
      { id : Id.CompTyp.t
      ; name : Name.t
      ; location : Location.t
      ; implicit_arguments : int
      ; kind : Comp.kind
      ; positivity : Sgn.positivity_flag
      ; constructors : Id.CompConst.t Name.Map.t
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
    entry |> constructors |> Name.Map.mem name

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
      ; destructors : Id.CompDest.t Name.Map.t
      }

    let make ~id ~name ~location ~implicit_arguments
        ?(destructors = Name.Map.empty) kind =
      { id; name; location; implicit_arguments; kind; destructors }

    let add_destructor ({ destructors; _ } as entry) name dest =
      { entry with destructors = Name.Map.add name dest destructors }
  end

  module Frozen = struct
    type t =
      { id : Id.CompTyp.t
      ; name : Name.t
      ; location : Location.t
      ; implicit_arguments : int
      ; kind : Comp.kind
      ; destructors : Id.CompDest.t Name.Map.t
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
    entry |> destructors |> Name.Map.mem name

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
    ; declarations : 'a Name.LinkedHamt.t
    }

  let make ~id ~location ?(declarations = Name.LinkedHamt.empty) name =
    { id; name; location; declarations }

  let[@inline] id { id; _ } = id

  let[@inline] location { location; _ } = location

  let[@inline] name { name; _ } = name

  let[@inline] declarations { declarations; _ } = declarations

  let lookup m name = m |> declarations |> Name.LinkedHamt.find_opt name
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
    { id : Id.DocumentationComment.t
    ; content : string
    ; location : Location.t
    }

  let make ~id ~location content = { id; content; location }

  let[@inline] id { id; _ } = id

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

  let make ~id ~location ?(name = Option.none) ~search_parameters query =
    { id; location; name; search_parameters; query }

  let[@inline] id { id; _ } = id

  let[@inline] location { location; _ } = location

  let[@inline] name { name; _ } = name

  let[@inline] query { query; _ } = query

  let[@inline] search_parameters { search_parameters; _ } = search_parameters

  module Ord : Support.Ord.ORD with type t = t =
    (val Ord.contramap (module Id.Query) id)

  module Set = Set.Make (Ord)

  include (Ord : Support.Ord.ORD with type t := t)
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

  let make ~id ~location ?(name = Option.none) ~search_parameters query =
    { id; location; name; search_parameters; query }

  let[@inline] id { id; _ } = id

  let[@inline] location { location; _ } = location

  let[@inline] name { name; _ } = name

  let[@inline] query { query; _ } = query

  let[@inline] search_parameters { search_parameters; _ } = search_parameters

  module Ord : Support.Ord.ORD with type t = t =
    (val Ord.contramap (module Id.MQuery) id)

  module Set = Set.Make (Ord)

  include (Ord : Support.Ord.ORD with type t := t)
end

  type mutually_recursive_typs =
    [ `Typs of (Typ.t * Const.t Name.LinkedHamt.t) Nonempty.t ]

  type mutually_recursive_comp_typs =
    [ `Comp_typs of
      [ `Comp_typ of CompTyp.t * CompConst.t Name.LinkedHamt.t
      | `Comp_cotyp of CompCotyp.t * CompDest.t Name.LinkedHamt.t
      ]
      Nonempty.t
    ]

  type mutually_recursive_programs =
    [ `Programs of Comp.t Name.LinkedHamt1.t ]

  (** The type of declarations in Beluga signatures. *)
  type declaration =
    [ `Typ_declaration of Typ.t Declaration.t
    | `Const_declaration of Const.t Declaration.t
    | `Comp_typ_declaration of CompTyp.t Declaration.t
    | `Comp_const_declaration of CompConst.t Declaration.t
    | `Comp_cotyp_declaration of CompCotyp.t Declaration.t
    | `Comp_dest_declaration of CompDest.t Declaration.t
    | `Comp_declaration of Comp.t Declaration.t
    | `Module_declaration of declaration Module.t Declaration.t
    | `Documentation_comment of DocumentationComment.t
    | `Mutually_recursive_declaration of
      [ mutually_recursive_typs
      | mutually_recursive_comp_typs
      | mutually_recursive_programs
      ]
    | `Query_declaration of Query.t Declaration.t
    | `MQuery_declaration of MQuery.t Declaration.t
    ]

  (** {1 Constructors} *)

  (** The empty Beluga signature. *)
  val empty : t

  val add_lf_family : t -> Typ.t -> t

  val add_lf_constant :
       t
    -> Const.t
    -> (t, [> `Frozen_typ_declaration_error of Id.Typ.t ]) Result.t

  val add_comp_typ : t -> CompTyp.t -> t

  val add_comp_constructor :
       t
    -> CompConst.t
    -> (t, [> `Frozen_comp_typ_declaration_error of Id.CompTyp.t ]) Result.t

  val add_comp_cotyp : t -> CompCotyp.t -> t

  val add_comp_destructor :
       t
    -> CompDest.t
    -> ( t
       , [> `Frozen_comp_cotyp_declaration_error of Id.CompCotyp.t ] )
       Result.t

  val add_comp : t -> Comp.t -> t

  val add_module : t -> declaration Module.t -> t

  val add_documentation_comment : t -> DocumentationComment.t -> t

  (** {1 Lookups by Qualified Name} *)

  (** [lookup signature name] returns [None] if there is no declaration in
      [signature] having name [name], and otherwise returns
      [Some (signature', declaration)] where [signature'] is the signature up
      to and including [declaration] and [declaration] is the latest
      declaration in [signature] having name [name]. *)
  val lookup : t -> QualifiedName.t -> (t * declaration) Option.t

  val lookup_lf_family : t -> QualifiedName.t -> (t * Typ.t) Option.t

  val lookup_lf_constant : t -> QualifiedName.t -> (t * Const.t) Option.t

  val lookup_comp_typ : t -> QualifiedName.t -> (t * CompTyp.t) Option.t

  val lookup_comp_constructor :
    t -> QualifiedName.t -> (t * CompConst.t) Option.t

  val lookup_comp_cotyp : t -> QualifiedName.t -> (t * CompCotyp.t) Option.t

  val lookup_comp_destructor :
    t -> QualifiedName.t -> (t * CompDest.t) Option.t

  val lookup_comp : t -> QualifiedName.t -> (t * Comp.t) Option.t

  val lookup_query : t -> QualifiedName.t -> (t * Query.t) Option.t

  val lookup_mquery : t -> QualifiedName.t -> (t * MQuery.t) Option.t

  (** {1 Scanning} *)

  val find_all_queries : t -> (t * Query.t) List.t

  val find_all_mqueries : t -> (t * MQuery.t) List.t

  (** {1 Iterators} *)

  (** [iter f signature] applies function [f] in turn on the declarations of
      [signature] in the order in which they appear in the source files. *)
  val iter : (t -> declaration -> unit) -> t -> unit

  (** [fold f init signature] reduces [signature] to a value by applying [f]
      in turn on the declarations of [signature] in the order in which they
      appear in the source files, starting with accumulator [init]. *)
  val fold : ('a -> t -> declaration -> 'a) -> 'a -> t -> 'a
end
