open Support

type plicity =
  | Implicit
  | Explicit

type t = plicity

let implicit = Implicit

let explicit = Explicit

let[@inline] fold ~implicit ~explicit = function
  | Explicit -> explicit ()
  | Implicit -> implicit ()

include Eq.Make (struct
  type nonrec t = t

  let equal p1 p2 =
    match (p1, p2) with
    | Explicit, Explicit -> true
    | Implicit, Implicit -> true
    | _, _ -> false
end)

let is_explicit = ( = ) explicit

let is_implicit = ( = ) implicit
