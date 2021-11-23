open Support

module Plicity = struct
  type t =
    | Implicit
    | Explicit

  let implicit = Implicit

  let explicit = Explicit

  let fold ~implicit ~explicit = function
    | Explicit ->
        explicit ()
    | Implicit ->
        implicit ()
end

include Plicity

include Eq.Make (struct
  include Plicity

  let equal p1 p2 =
    match (p1, p2) with
    | Explicit, Explicit ->
        true
    | Implicit, Implicit ->
        true
    | _, _ ->
        false
end)

let is_explicit = equal explicit

let is_implicit = equal implicit
