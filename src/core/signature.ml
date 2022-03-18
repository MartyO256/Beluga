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
