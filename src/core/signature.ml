(** Beluga signatures.

    @author Marc-Antoine Ouimet *)

open Support

module Name = struct
  type t = string

  module Eq : Eq.EQ with type t := t = Eq.Make (String)

  module Ord : Ord.ORD with type t := t = Ord.Make (String)

  module Show : Show.SHOW with type t := string = struct
    let pp ppf name = Format.fprintf ppf "%s" name

    let show = Fun.id
  end

  include Eq
  include Ord
  include Show
  module Set = Set.Make (String)
  module Map = Map.Make (String)

  type fresh_name_supplier = Set.t -> t

  (** [find_distinct names used_names] is the first name in [names] that is
      not a member of [used_names]. The sequence [names] is assumed to be
      infinite, and that the elements therein are all distinct. *)
  let rec find_distinct : t Seq.t -> Set.t -> t =
   fun generate_name used_names ->
    match generate_name () with
    | Seq.Nil -> raise @@ Invalid_argument "Finite name sequence"
    | Seq.Cons (hd, tl) ->
      if Set.mem hd used_names then find_distinct tl used_names else hd

  (** [names_seq base index] is the infinite sequence of names with prefix
      [base] and incremental integer suffix starting with [index].

      For instance, [names_seq "x" 1] is the sequence of names
      [\["x1"; "x2"; ...\]].

      The output sequence raises when the maximum index is reached so that
      [Seq.Nil] is not reachable. *)
  let rec names_seq : string -> int -> t Seq.t =
   fun base i ->
    if Int.equal i Int.max_int then
      raise @@ Invalid_argument "Maximum int reached"
    else
      Seq.cons (base ^ string_of_int i) (fun () -> names_seq base (i + 1) ())

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

  module Show : Show.SHOW with type t := t = Show.Make (struct
    type nonrec t = t

    let pp ppf n =
      Format.fprintf ppf "%a::%a"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "::")
           (fun ppf x -> Format.fprintf ppf "%a" Name.pp x))
        (modules n) Name.pp (name n)
  end)

  module Eq : Eq.EQ with type t := t = Eq.Make (struct
    type nonrec t = t

    let equal x y =
      List.equal Name.equal (modules x) (modules y)
      && Name.equal (name x) (name y)
  end)

  module Ord' : Ord.ORD with type t = t = Ord.Make (struct
    type nonrec t = t

    module ModuleListOrd : Ord.ORD with type t = Name.t list =
      List.MakeOrd (Name)

    let compare x y =
      let comparison = ModuleListOrd.compare (modules x) (modules y) in
      if Stdlib.(comparison <> 0) then comparison
      else Name.compare (name x) (name y)
  end)

  module Ord : Ord.ORD with type t := t = Ord'

  include Show
  include Eq
  include Ord
  module Set = Set.Make (Ord')
  module Map = Map.Make (Ord')
end

module Declaration = struct
  type name = Name.t

  type 'entry t =
    { name : name
    ; entry : 'entry
    }

  let[@inline] make name entry = { name; entry }

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

(** Unique identifiers for declarations in a signature as integers.

    This module type enables internal modules to construct IDs as integers.
    The type for IDs remains abstract in exported module signatures. *)
module type INTERNAL_ID = ID with type t = int

(** Base implementation for IDs as integers. *)
module BaseId : sig
  include INTERNAL_ID

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
  module CompConst = BaseId
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
    }

  let make ~id ~name ~location ~implicit_arguments typ =
    { id; name; location; implicit_arguments; typ }

  let[@inline] id { id; _ } = id

  let[@inline] location { location; _ } = location

  let[@inline] name { name; _ } = name

  let[@inline] typ { typ; _ } = typ
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

module BelugaDeclaration = struct
  module Typ = struct
    type t = [ `Typ_declaration of Typ.t Declaration.t ]
  end

  module Const = struct
    type t = [ `Const_declaration of Const.t Declaration.t ]
  end
end

module type BELUGA_SIGNATURE = sig
  (** The type of Beluga signatures. *)
  type t

  (** The type of declarations in Beluga signatures. *)
  type declaration =
    [ BelugaDeclaration.Typ.t
    | BelugaDeclaration.Const.t
    ]

  (** {1 Constructors} *)

  (** The empty Beluga signature. *)
  val empty : t

  val add_lf_constant :
    t -> Const.t -> (t, [> `Frozen_LF_family of Id.Typ.t ]) Result.t

  (** {1 Lookups} *)

  (** [lookup signature name] returns [None] if there is no declaration in
      [signature] having name [name], and otherwise returns
      [Some (signature', declaration)] where [signature'] is the signature up
      to and including [declaration] and [declaration] is the latest
      declaration in [signature] having name [name]. *)
  val lookup : t -> QualifiedName.t -> (t * declaration) Option.t

  val lookup_lf_family : t -> QualifiedName.t -> (t * Typ.t) Option.t

  val lookup_lf_const : t -> QualifiedName.t -> (t * Const.t) Option.t

  (** {1 Iterators} *)

  (** [iter f signature] applies function [f] in turn on the declarations of
      [signature] in the order in which they appear in the source files. *)
  val iter : (t -> declaration -> unit) -> t -> unit

  (** [fold f init signature] reduces [signature] to a value by applying [f]
      in turn on the declarations of [signature] in the order in which they
      appear in the source files, starting with accumulator [init]. *)
  val fold : ('a -> t -> declaration -> 'a) -> 'a -> t -> 'a
end
