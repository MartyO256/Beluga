(** Beluga signatures.

    @author Marc-Antoine Ouimet *)

open Support

(** Bound variable names.

    These are totally ordered for efficient lookups in map data structures.

    For signatures, a name is typically a string. *)
module type NAME = sig
  (** The type of names for bound variables. *)
  type t

  (** {1 Instances} *)

  include Eq.EQ with type t := t

  include Ord.ORD with type t := t

  include Show.SHOW with type t := t

  (** {1 Collections} *)

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end

(** Namespaced bound variable names.

    These are names for referring to bound variable names nested in modules. *)
module type QUALIFIED_NAME = sig
  (** The type of names for referring to names in the current module or in a
      different module. *)
  type t

  (** The type of names for modules and declarations. *)
  type name

  (** {1 Constructors} *)

  (** [make ms n] is the qualified name with name [n] when successively
      opening the modules named [ms]. *)
  val make : ?modules:name List.t -> name -> t

  (** {1 Destructors} *)

  (** [name n] is the declaration name referred to by [n]. *)
  val name : t -> name

  (** [modules n] is the list of module names for modules to open to refer to
      [n] in the module opening order. *)
  val modules : t -> name List.t

  (** {1 Instances} *)

  include Show.SHOW with type t := t

  include Eq.EQ with type t := t

  include Ord.ORD with type t := t

  (** {1 Collections} *)

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end

(** Bindings of entries to names. *)
module type DECLARATION = sig
  (** The type of declarations for bound variables referring to signature
      entries. *)
  type +'entry t

  (** The type of bound variable names for a declaration.

      This is the domain of signature declarations. *)
  type name

  (** {1 Constructors} *)

  (** [make name entry] is the declaration having name [name] and entry
      [entry]. *)
  val make : name -> 'entry -> 'entry t

  (** {1 Destructors} *)

  (** [name declaration] is the variable name bound by [declaration]. *)
  val name : 'entry t -> name

  (** [entry declaration] is the entry referred to by [declaration]. *)
  val entry : 'entry t -> 'entry
end

module Name : sig
  include NAME

  (** The type of supplier for a name that does not appear in a given set of
      used names. *)
  type fresh_name_supplier = Set.t -> t

  (** [prefixed_fresh_name_supplier base] is the fresh name supplier for
      names prefixed by [base] and optionally having an integer suffix. *)
  val prefixed_fresh_name_supplier : string -> fresh_name_supplier
end = struct
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

module QualifiedName (Name : NAME) : QUALIFIED_NAME with type name = Name.t =
struct
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
  module Make (Name : NAME) : DECLARATION with type name = Name.t = struct
    type name = Name.t

    type 'entry t =
      { name : name
      ; entry : 'entry
      }

    let[@inline] make name entry = { name; entry }

    let[@inline] name { name; _ } = name

    let[@inline] entry { entry; _ } = entry
  end
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
end

(** LF type family declarations. *)
module Typ : sig
  open Syntax.Int

  type t

  (** {1 Constructors} *)

  val make_initial_entry :
       id:int
    -> name:Name.t
    -> location:Location.t
    -> implicit_arguments:int
    -> LF.kind
    -> t

  (** {1 Destructors} *)

  val id : t -> Id.Typ.t

  val name : t -> Name.t

  val kind : t -> LF.kind

  (** {1 Freezing} *)

  val is_frozen : t -> bool

  val is_unfrozen : t -> bool

  val freeze :
       subordinates:Id.Typ.Set.t
    -> type_subordinated:Id.Typ.Set.t
    -> t
    -> (t, [> `Frozen_declaration_error of Id.Typ.t ]) Result.t

  (** {1 LF Constructors} *)

  val add_constructor :
       Name.t
    -> Id.Const.t
    -> t
    -> (t, [> `Frozen_declaration_error of Id.Typ.t ]) result

  val constructors : t -> Id.Const.t Name.Map.t

  val has_constructor_with_name : Name.t -> t -> bool

  (** {1 Naming} *)

  val fresh_var_name :
    t -> ?default_base_name:string -> Name.fresh_name_supplier

  val fresh_mvar_name :
    t -> ?default_base_name:string -> Name.fresh_name_supplier

  val set_var_naming_convention : Name.t option -> t -> t

  val set_mvar_naming_convention : Name.t option -> t -> t

  val set_naming_conventions :
    var:Name.t option -> mvar:Name.t option -> t -> t

  (** {1 Subordination} *)

  val is_subordinate :
       t
    -> Id.Typ.t
    -> (bool, [> `Unfrozen_declaration_error of Id.Typ.t ]) Result.t

  val is_type_subordinated :
       t
    -> Id.Typ.t
    -> (bool, [> `Unfrozen_declaration_error of Id.Typ.t ]) Result.t
end = struct
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

    let add_constructor entry name const =
      { entry with
        constructors = Name.Map.add name const entry.constructors
      }

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
    | Frozen { Frozen.id; _ } -> Result.error @@ `Frozen_declaration_error id
    | Unfrozen entry -> Result.ok @@ f entry

  let if_frozen f = function
    | Frozen entry -> Result.ok @@ f entry
    | Unfrozen { Unfrozen.id; _ } ->
      Result.error @@ `Unfrozen_declaration_error id

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

(** LF type constant declarations. *)
module Const : sig
  open Syntax.Int

  type t

  val name : t -> Name.t

  val typ : t -> LF.typ
end = struct
  open Syntax.Int

  type t =
    { name : Name.t
    ; location : Location.t
    ; implicit_arguments : int
    ; typ : LF.typ
    }

  let[@inline] name { name; _ } = name

  let[@inline] typ { typ; _ } = typ
end

module BelugaDeclaration = struct
  module Declaration = Declaration.Make (Name)

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

  (** The type of bound names in Beluga signatures. *)
  type name

  (** The type of namespaced bound names in Beluga signatures. *)
  type qualified_name

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
  val lookup : t -> qualified_name -> (t * declaration) Option.t

  val lookup_lf_family : t -> qualified_name -> (t * Typ.t) Option.t

  val lookup_lf_const : t -> qualified_name -> (t * Const.t) Option.t

  (** {1 Iterators} *)

  (** [iter f signature] applies function [f] in turn on the declarations of
      [signature] in the order in which they appear in the source files. *)
  val iter : (t -> declaration -> unit) -> t -> unit

  (** [fold f init signature] reduces [signature] to a value by applying [f]
      in turn on the declarations of [signature] in the order in which they
      appear in the source files, starting with accumulator [init]. *)
  val fold : ('a -> t -> declaration -> 'a) -> 'a -> t -> 'a
end
