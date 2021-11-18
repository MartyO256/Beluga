open Support

(** @author Marc-Antoine Ouimet *)
module AbstractSignature (Name : sig
  type t

  include Map.OrderedType with type t := t
end) (Declaration : sig
  type t

  val name : t -> Name.t
end) : sig
  (** The type of abstract signatures. *)
  type t

  (** The empty abstract signature. *)
  val empty : t

  (** [add signature declaration] is the abstract signature constructed by
      adding [declaration] to [signature]. *)
  val add : t -> Declaration.t -> t

  (** [lookup signature name] returns [None] if there is no declaration in
      [signature] having name [name], and otherwise returns
      [Some (signature', declaration)] where [signature'] is the signature up
      to and including [declaration] and [declaration] is the latest
      declaration in [signature] having name [name]. *)
  val lookup : t -> Name.t -> (t * Declaration.t) option

  (** [iter f signature] applies function [f] in turn on the declarations of
      [signature] in the order in which they appear in the source files. *)
  val iter : (t * Declaration.t -> unit) -> t -> unit
end = struct
  module NameMap = Map.Make (Name)

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
    let rec bound_declaration = signature', declaration
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

  let lookup {bindings; _} name =
    NameMap.find_opt name (Lazy.force bindings)

  let iter f {declarations; _} =
    List.iter_rev f declarations
end
