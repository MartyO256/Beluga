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
  type t

  (** The type of bound variable names for a declaration.

      This is the domain of signature declarations. *)
  type name

  (** The type of entries referred to by name for a declaration.

      This is the range of signature declarations. *)
  type entry

  (** {1 Constructors} *)

  (** [make name entry] is the declaration having name [name] and entry
      [entry]. *)
  val make : name -> entry -> t

  (** {1 Destructors} *)

  (** [name declaration] is the variable name bound by [declaration]. *)
  val name : t -> name

  (** [entry declaration] is the entry referred to by [declaration]. *)
  val entry : t -> entry
end

module type ABSTRACT_SIGNATURE = sig
  (** The type of abstract signatures. *)
  type t

  (** The type of bound names in the abstract signature. *)
  type name

  (** The type of signature entries. *)
  type entry

  (** The type of declarations in the abstract signature. *)
  type declaration

  (** {1 Constructors} *)

  (** The empty abstract signature. *)
  val empty : t

  (** [add signature declaration] is the abstract signature constructed by
      adding [declaration] to [signature]. *)
  val add : t -> declaration -> t

  (** {1 Utils} *)

  (** [lookup signature name] returns [None] if there is no declaration in
      [signature] having name [name], and otherwise returns
      [Some (signature', declaration)] where [signature'] is the signature up
      to and including [declaration] and [declaration] is the latest
      declaration in [signature] having name [name]. *)
  val lookup : t -> name -> (t * declaration) Option.t

  (** [is_bound signature name] is [true] if [name] appears in a declaration
      in [signature], and [false] otherwise. *)
  val is_bound : t -> name -> bool

  (** The logical negation of {!is_bound}. *)
  val is_unbound : t -> name -> bool

  (** {1 Iterators} *)

  (** [iter f signature] applies function [f] in turn on the declarations of
      [signature] in the order in which they appear in the source files. *)
  val iter : (t -> declaration -> unit) -> t -> unit

  (** [fold f init signature] reduces [signature] to a value by applying [f]
      in turn on the declarations of [signature] in the order in which they
      appear in the source files, starting with accumulator [init]. *)
  val fold : ('a -> t -> declaration -> 'a) -> 'a -> t -> 'a
end

module AbstractSignature
    (Name : NAME)
    (Declaration : DECLARATION with type name = Name.t) :
  ABSTRACT_SIGNATURE
    with type name = Name.t
     and type entry = Declaration.entry
     and type declaration = Declaration.t = struct
  module NameMap = Map.Make (Name)

  type name = Declaration.name

  type entry = Declaration.entry

  type declaration = Declaration.t

  type t =
    { declarations : bound_declaration list
          (** The list of bound declarations in the signature in reverse
              order of appearance in the source files. *)
    ; bindings : bound_declaration NameMap.t Lazy.t
          (** The bound declarations in the signature mapped by the
              declaration's name. *)
    }

  (** The type of signature declarations recursively constructed with the
      signature up to and including the declaration. *)
  and bound_declaration = t * Declaration.t

  let empty = { declarations = []; bindings = lazy NameMap.empty }

  let add signature declaration =
    let rec bound_declaration = (signature', declaration)
    and signature' =
      { declarations = bound_declaration :: signature.declarations
      ; bindings =
          lazy
            (NameMap.add
               (Declaration.name declaration)
               bound_declaration
               (Lazy.force signature.bindings))
      }
    in
    signature'

  let lookup { bindings; _ } name =
    Lazy.force bindings |> NameMap.find_opt name

  let is_bound signature name =
    lookup signature name |> Option.fold ~none:false ~some:(fun _ -> true)

  let is_unbound signature name = not @@ is_bound signature name

  let iter f { declarations; _ } =
    List.iter_rev
      (fun (signature, declaration) -> f signature declaration)
      declarations

  let fold f init { declarations; _ } =
    List.fold_right
      (fun (signature, declaration) acc -> f acc signature declaration)
      declarations init
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
  module Make
      (Name : NAME) (Entry : sig
        type t
      end) : DECLARATION with type name = Name.t and type entry = Entry.t =
  struct
    type name = Name.t

    type entry = Entry.t

    type t =
      { name : name
      ; entry : entry
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
module BaseID : INTERNAL_ID = struct
  include Int
  include Ord.Make (Int)
  module Set = Set.Make (Int)
  module Map = Map.Make (Int)
end

