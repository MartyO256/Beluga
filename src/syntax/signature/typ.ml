open Beluga
open Support
open Common
open Internal

module Kind = struct
  (** [arguments tK] is [(i, e, t)] where

      - [i] is the number of implicit arguments in [tK]
      - [e] is the number of explicit and inductive arguments in [tK]
      - [t] is the total number of implicit, explicit and inductive arguments
        in [tK] *)
  let arguments =
    let rec arguments (implicit, explicit) tK =
      match tK with
      | LF.Typ -> (implicit, explicit)
      | LF.PiKind ((_, Depend.Explicit), tK')
      | LF.PiKind ((_, Depend.Inductive), tK') ->
        arguments (implicit, 1 + explicit) tK'
      | LF.PiKind ((_, Depend.Implicit), tK') ->
        arguments (1 + implicit, explicit) tK'
    in
    fun tK ->
      let implicit, explicit = arguments (0, 0) tK in
      (implicit, explicit, implicit + explicit)
end

module Unfrozen = struct
  type t =
    { id : Id.Typ.t
    ; name : Name.t
    ; location : Location.t
    ; arguments : int
    ; implicit_arguments : int
    ; explicit_arguments : int
    ; kind : LF.kind
    ; var_name_base : string Option.t
    ; mvar_name_base : string Option.t
    ; constructors : Id.Const.t Name.Hamt.t
    ; documentation_comment : DocumentationComment.t Option.t
    }

  let make ~id ~name ~location ?var_name_base ?mvar_name_base
      ?(constructors = Name.Hamt.empty)
      ?(documentation_comment = Option.none) kind =
    let implicit_arguments, explicit_arguments, arguments =
      Kind.arguments kind
    in
    { id
    ; name
    ; location
    ; implicit_arguments
    ; explicit_arguments
    ; arguments
    ; kind
    ; var_name_base
    ; mvar_name_base
    ; constructors
    ; documentation_comment
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
    ; arguments : int
    ; implicit_arguments : int
    ; explicit_arguments : int
    ; kind : LF.kind
    ; var_name_base : string Option.t
    ; mvar_name_base : string Option.t
    ; constructors : Id.Const.t Name.Hamt.t
    ; term_subordinates : Id.Typ.Set.t
    ; type_subordinated_to : Id.Typ.Set.t
    ; documentation_comment : DocumentationComment.t Option.t
    }

  let[@inline] term_subordinates { term_subordinates; _ } = term_subordinates

  let[@inline] type_subordinated_to { type_subordinated_to; _ } =
    type_subordinated_to

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

let[@inline] arguments = function
  | Frozen { Frozen.arguments; _ } | Unfrozen { Unfrozen.arguments; _ } ->
    arguments

let[@inline] implicit_arguments = function
  | Frozen { Frozen.implicit_arguments; _ }
  | Unfrozen { Unfrozen.implicit_arguments; _ } -> implicit_arguments

let[@inline] explicit_arguments = function
  | Frozen { Frozen.explicit_arguments; _ }
  | Unfrozen { Unfrozen.explicit_arguments; _ } -> explicit_arguments

let[@inline] var_name_base = function
  | Frozen { Frozen.var_name_base; _ }
  | Unfrozen { Unfrozen.var_name_base; _ } -> var_name_base

let[@inline] mvar_name_base = function
  | Frozen { Frozen.mvar_name_base; _ }
  | Unfrozen { Unfrozen.mvar_name_base; _ } -> mvar_name_base

let[@inline] constructors = function
  | Frozen { Frozen.constructors; _ } | Unfrozen { Unfrozen.constructors; _ }
    -> constructors

let[@inline] documentation_comment = function
  | Frozen { Frozen.documentation_comment; _ }
  | Unfrozen { Unfrozen.documentation_comment; _ } -> documentation_comment

let make_initial_declaration ~id ~name ~location ?documentation_comment kind
    =
  Unfrozen (Unfrozen.make ~id ~name ~location ~documentation_comment kind)

let is_frozen = function
  | Frozen _ -> true
  | Unfrozen _ -> false

let is_unfrozen entry = not @@ is_frozen entry

exception FrozenTyp of t

let if_unfrozen f = function
  | Frozen _ as tA -> raise @@ FrozenTyp tA
  | Unfrozen entry -> f entry

exception UnfrozenTyp of t

let if_frozen f = function
  | Frozen entry -> f entry
  | Unfrozen _ as tA -> raise @@ UnfrozenTyp tA

let has_constructor_with_name name = Fun.(constructors >> Name.Hamt.mem name)

exception TypNameCollision of Name.t * Id.Const.t * t

exception ConstNameCollision of Name.t * Id.Const.t * t

let add_constructor tM_name tM tA =
  if Name.(name tA <> tM_name) then
    raise @@ TypNameCollision (tM_name, tM, tA);
  if has_constructor_with_name tM_name tA then
    raise @@ ConstNameCollision (tM_name, tM, tA);
  tA
  |> if_unfrozen (fun tA ->
         Unfrozen (Unfrozen.add_constructor tA tM_name tM))

let frozen ~term_subordinates ~type_subordinated_to
    { Unfrozen.id
    ; name
    ; location
    ; arguments
    ; implicit_arguments
    ; explicit_arguments
    ; kind
    ; var_name_base
    ; mvar_name_base
    ; constructors
    ; documentation_comment
    } =
  { Frozen.id
  ; name
  ; location
  ; arguments
  ; implicit_arguments
  ; explicit_arguments
  ; kind
  ; constructors
  ; var_name_base
  ; mvar_name_base
  ; term_subordinates
  ; type_subordinated_to
  ; documentation_comment
  }

let freeze ~term_subordinates ~type_subordinated_to =
  if_unfrozen (fun x ->
      Frozen (frozen ~term_subordinates ~type_subordinated_to x))

let fresh_var_name ?(base_name = "x") entry =
  entry |> var_name_base
  |> Option.value ~default:base_name
  |> Name.prefixed_fresh_name_supplier

let fresh_mvar_name ?(base_name = "X") entry =
  entry |> mvar_name_base
  |> Option.value ~default:base_name
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

let is_term_subordinate entry typ =
  entry |> if_frozen Fun.(Frozen.term_subordinates >> Id.Typ.Set.mem typ)

let is_type_subordinate_to entry typ =
  entry |> if_frozen Fun.(Frozen.type_subordinated_to >> Id.Typ.Set.mem typ)
