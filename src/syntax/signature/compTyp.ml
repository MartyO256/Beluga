open Support
open Common
open Internal

module Unfrozen = struct
  type t =
    { id : Id.CompTyp.t
    ; name : Name.t
    ; location : Location.t
    ; implicit_arguments : int
    ; kind : Comp.kind
    ; positivity : Sgn.positivity_flag
    ; constructors : Id.CompConst.t Name.Hamt.t
    ; documentation_comment : DocumentationComment.t Option.t
    }

  let make ~id ~name ~location ~implicit_arguments ~positivity
      ?(constructors = Name.Hamt.empty)
      ?(documentation_comment = Option.none) kind =
    { id
    ; name
    ; location
    ; implicit_arguments
    ; kind
    ; positivity
    ; constructors
    ; documentation_comment
    }

  let add_constructor ({ constructors; _ } as entry) name const =
    { entry with constructors = Name.Hamt.add name const constructors }
end

module Frozen = struct
  type t =
    { id : Id.CompTyp.t
    ; name : Name.t
    ; location : Location.t
    ; implicit_arguments : int
    ; kind : Comp.kind
    ; positivity : Sgn.positivity_flag
    ; constructors : Id.CompConst.t Name.Hamt.t
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

let[@inline] constructors = function
  | Frozen { Frozen.constructors; _ } | Unfrozen { Unfrozen.constructors; _ }
    -> constructors

let[@inline] documentation_comment = function
  | Frozen { Frozen.documentation_comment; _ }
  | Unfrozen { Unfrozen.documentation_comment; _ } -> documentation_comment

let make_initial_declaration ~id ~name ~location ~implicit_arguments
    ~positivity ?documentation_comment kind =
  Unfrozen
    (Unfrozen.make ~id ~name ~location ~implicit_arguments ~positivity
       ~documentation_comment kind)

let is_frozen = function
  | Frozen _ -> true
  | Unfrozen _ -> false

let is_unfrozen entry = not @@ is_frozen entry

exception FrozenCompTyp of t

let if_unfrozen f = function
  | Frozen _ as cA -> raise @@ FrozenCompTyp cA
  | Unfrozen entry -> f entry

exception UnfrozenCompTyp of t

let if_frozen f = function
  | Frozen entry -> f entry
  | Unfrozen _ as cA -> raise @@ UnfrozenCompTyp cA

let has_constructor_with_name name = Fun.(constructors >> Name.Hamt.mem name)

exception CompTypNameCollision of Name.t * Id.CompConst.t * t

exception CompConstNameCollision of Name.t * Id.CompConst.t * t

let add_constructor cM_name cM cA =
  if Name.(name cA <> cM_name) then
    raise @@ CompTypNameCollision (cM_name, cM, cA);
  if has_constructor_with_name cM_name cA then
    raise @@ CompConstNameCollision (cM_name, cM, cA);
  cA
  |> if_unfrozen (fun x -> Unfrozen (Unfrozen.add_constructor x cM_name cM))

let frozen
    { Unfrozen.id
    ; name
    ; location
    ; implicit_arguments
    ; kind
    ; positivity
    ; constructors
    ; documentation_comment
    } =
  { Frozen.id
  ; name
  ; location
  ; implicit_arguments
  ; kind
  ; positivity
  ; constructors
  ; documentation_comment
  }

let freeze = if_unfrozen (fun x -> Frozen (frozen x))
