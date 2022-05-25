open Support
open Common
open Internal

module Unfrozen = struct
  type t =
    { id : Id.CompCotyp.t
    ; name : Name.t
    ; location : Location.t
    ; implicit_arguments : int
    ; kind : Comp.kind
    ; destructors : Id.CompDest.t Name.Hamt.t
    ; documentation_comment : DocumentationComment.t Option.t
    }

  let make ~id ~name ~location ~implicit_arguments
      ?(destructors = Name.Hamt.empty) ?(documentation_comment = Option.none)
      kind =
    { id
    ; name
    ; location
    ; implicit_arguments
    ; kind
    ; destructors
    ; documentation_comment
    }

  let add_destructor ({ destructors; _ } as entry) name dest =
    { entry with destructors = Name.Hamt.add name dest destructors }
end

module Frozen = struct
  type t =
    { id : Id.CompCotyp.t
    ; name : Name.t
    ; location : Location.t
    ; implicit_arguments : int
    ; kind : Comp.kind
    ; destructors : Id.CompDest.t Name.Hamt.t
    ; documentation_comment : DocumentationComment.t Option.t
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

let[@inline] documentation_comment = function
  | Frozen { Frozen.documentation_comment; _ }
  | Unfrozen { Unfrozen.documentation_comment; _ } -> documentation_comment

let make_initial_declaration ~id ~name ~location ~implicit_arguments
    ?documentation_comment kind =
  Unfrozen
    (Unfrozen.make ~id ~name ~location ~implicit_arguments
       ~documentation_comment kind)

let is_frozen = function
  | Frozen _ -> true
  | Unfrozen _ -> false

let is_unfrozen entry = not @@ is_frozen entry

exception FrozenCompCotyp of t

let if_unfrozen f = function
  | Frozen _ as cA -> raise @@ FrozenCompCotyp cA
  | Unfrozen entry -> f entry

exception UnfrozenCompCotyp of t

let if_frozen f = function
  | Frozen entry -> f entry
  | Unfrozen _ as cA -> raise @@ UnfrozenCompCotyp cA

let has_destructor_with_name name = Fun.(destructors >> Name.Hamt.mem name)

exception CompCotypNameCollision of Name.t * Id.CompDest.t * t

exception CompDestNameCollision of Name.t * Id.CompDest.t * t

let add_destructor cM_name cM cA =
  if Name.(name cA <> cM_name) then
    raise @@ CompCotypNameCollision (cM_name, cM, cA);
  if has_destructor_with_name cM_name cA then
    raise @@ CompDestNameCollision (cM_name, cM, cA);
  cA
  |> if_unfrozen (fun x -> Unfrozen (Unfrozen.add_destructor x cM_name cM))

let frozen
    { Unfrozen.id
    ; name
    ; location
    ; implicit_arguments
    ; kind
    ; destructors
    ; documentation_comment
    } =
  { Frozen.id
  ; name
  ; location
  ; implicit_arguments
  ; kind
  ; destructors
  ; documentation_comment
  }

let freeze = if_unfrozen (fun x -> Frozen (frozen x))
