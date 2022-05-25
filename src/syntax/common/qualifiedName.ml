open Support

type t =
  { name : Name.t
  ; modules : Name.t List.t
  }

let make ?(modules = []) name = { name; modules }

let[@inline] name { name; _ } = name

let[@inline] modules { modules; _ } = modules

include (
  Show.Make (struct
    type nonrec t = t

    let pp ppf n =
      Format.fprintf ppf "%a::%a"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "::")
           (fun ppf x -> Format.fprintf ppf "%a" Name.pp x))
        (modules n) Name.pp (name n)
  end) :
    Show.SHOW with type t := t)

include (
  Eq.Make (struct
    type nonrec t = t

    let equal x y =
      if Name.equal (name x) (name y) then
        List.equal Name.equal (modules x) (modules y)
      else false
  end) :
    Eq.EQ with type t := t)

module Ord : Ord.ORD with type t = t = Ord.Make (struct
  type nonrec t = t

  module ModuleListOrd : Ord.ORD with type t = Name.t list =
    List.MakeOrd (Name)

  let compare x y =
    let comparison = ModuleListOrd.compare (modules x) (modules y) in
    if Stdlib.(comparison <> 0) then comparison
    else Name.compare (name x) (name y)
end)

include (Ord : Support.Ord.ORD with type t := t)

module Set = Set.Make (Ord)
module Map = Map.Make (Ord)
