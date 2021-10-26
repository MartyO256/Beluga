open Support

type t =
  | Implicit
  | Explicit

let implicit = Implicit

let explicit = Explicit

let fold ~implicit:on_implicit ~explicit:on_explicit = function
  | Explicit -> on_explicit ()
  | Implicit -> on_implicit ()

module Eq = Eq.Make (struct
  type tmp = t (* Workaround `type t = t` being recursive *)

  type t = tmp

  let equal p1 p2 =
    match p1, p2 with
    | Explicit, Explicit -> true
    | Implicit, Implicit -> true
    | _, _ -> false
end)

let is_explicit = Eq.equal explicit

let is_implicit = Eq.equal implicit
