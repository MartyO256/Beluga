(** Beluga signatures.

    @author Marc-Antoine Ouimet *)

open Support

module type NAME = sig
  type t

  include Ord.ORD with type t := t
end

module type DECLARATION = sig
  type name

  type t

  val name : t -> name
end

module type ABSTRACT_SIGNATURE = sig
  (** The type of bound names in the abstract signature. *)
  type name

  (** The type of declarations in the abstract signature. *)
  type declaration

  (** The type of abstract signatures. *)
  type t

  (** The empty abstract signature. *)
  val empty : t

  (** [add signature declaration] is the abstract signature constructed by
      adding [declaration] to [signature]. *)
  val add : t -> declaration -> t

  (** [lookup signature name] returns [None] if there is no declaration in
      [signature] having name [name], and otherwise returns
      [Some (signature', declaration)] where [signature'] is the signature up
      to and including [declaration] and [declaration] is the latest
      declaration in [signature] having name [name]. *)
  val lookup : t -> name -> (t * declaration) option

  (** [iter f signature] applies function [f] in turn on the declarations of
      [signature] in the order in which they appear in the source files. *)
  val iter : (t * declaration -> unit) -> t -> unit
end

module AbstractSignature
    (Name : NAME)
    (Declaration : DECLARATION with type name = Name.t) :
  ABSTRACT_SIGNATURE
    with type declaration = Declaration.t
     and type name = Name.t = struct
  module NameMap = Map.Make (Name)

  type declaration = Declaration.t

  type name = Name.t

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
    NameMap.find_opt name (Lazy.force bindings)

  let iter f { declarations; _ } = List.iter_rev f declarations
end
