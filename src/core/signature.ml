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
end
