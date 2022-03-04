(** Beluga signatures.

    @author Marc-Antoine Ouimet *)

open Support

(** Bound variable names.

    Bound variable names are totally ordered for efficient lookups in map
    data structures.

    For signatures, a name is typically a string. *)
module type NAME = sig
  (** The type of names for bound variables. *)
  type t

  (** {1 Instances} *)

  include Ord.ORD with type t := t
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

  (** {1 Destructors} *)

  (** [name declaration] is the variable name bound by [declaration]. *)
  val name : t -> name

  (** [entry declaration] is the entry referred to by [declaration]. *)
  val entry : t -> entry
end

module type ABSTRACT_SIGNATURE = sig
  (** The type of bound names in the abstract signature. *)
  type name

  (** The type of signature entries. *)
  type entry

  (** The type of declarations in the abstract signature. *)
  type declaration

  (** The type of abstract signatures. *)
  type t

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
  val lookup : t -> name -> (t * declaration) option

  (** [is_bound signature name] is [true] if [name] appears in a declaration
      in [signature], and [false] otherwise. *)
  val is_bound : t -> name -> bool

  (** The logical negation of {!is_bound}. *)
  val is_unbound : t -> name -> bool

  (** {1 Traversals} *)

  (** [iter f signature] applies function [f] in turn on the declarations of
      [signature] in the order in which they appear in the source files. *)
  val iter : (t * declaration -> unit) -> t -> unit
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

  let iter f { declarations; _ } = List.iter_rev f declarations
end
