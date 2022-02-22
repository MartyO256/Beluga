open Support

module Base = struct
  type t =
    | Implicit
    | Explicit
    | Inductive

  let implicit = Implicit

  let explicit = Explicit

  let inductive = Inductive

  let of_plicity =
    Plicity.fold
      ~implicit:(fun () -> implicit)
      ~explicit:(fun () -> explicit)

  let to_plicity = function
    | Implicit -> Plicity.implicit
    | Explicit -> Plicity.explicit
    | Inductive ->
      Error.violation "[Depend] [to_plicity] Inductive is impossible"

  let to_plicity' = function
    | Inductive -> Plicity.explicit
    | d -> to_plicity d

  let max d1 d2 =
    match (d1, d2) with
    | Explicit, Explicit -> explicit
    | _ -> implicit

  let is_explicit' = Fun.(Plicity.is_explicit ++ to_plicity')

  let fold ~implicit ~explicit ~inductive = function
    | Implicit -> implicit ()
    | Explicit -> explicit ()
    | Inductive -> inductive ()
end

include Base

include Eq.Make (struct
  include Base

  let equal d1 d2 =
    match (d1, d2) with
    | Implicit, Implicit -> true
    | Explicit, Explicit -> true
    | Inductive, Inductive -> true
    | _ -> false
end)

let is_implicit = ( = ) implicit

let is_explicit = ( = ) explicit

let is_inductive = ( = ) inductive
