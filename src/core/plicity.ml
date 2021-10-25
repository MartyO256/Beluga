type t =
  | Implicit
  | Explicit

let implicit = Implicit

let explicit = Explicit

let is_explicit = function
  | Explicit -> true
  | Implicit -> false

let is_implicit = function
  | Implicit -> true
  | Explicit -> false

let equal p1 p2 =
  match p1, p2 with
  | Explicit, Explicit -> true
  | Implicit, Implicit -> true
  | _, _ -> false

let fold ~implicit:on_implicit ~explicit:on_explicit = function
  | Explicit -> on_explicit ()
  | Implicit -> on_implicit ()
