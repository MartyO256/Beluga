open Support

type plicity =
  | Implicit
  | Explicit

module Base = struct
  type t = plicity

  let implicit = Implicit

  let explicit = Explicit

  let[@inline] fold ~implicit ~explicit = function
    | Explicit -> explicit ()
    | Implicit -> implicit ()
end

include Base

include Eq.Make (struct
  include Base

  let equal p1 p2 =
    match (p1, p2) with
    | Explicit, Explicit -> true
    | Implicit, Implicit -> true
    | _, _ -> false
end)

let is_explicit = ( = ) explicit

let is_implicit = ( = ) implicit
