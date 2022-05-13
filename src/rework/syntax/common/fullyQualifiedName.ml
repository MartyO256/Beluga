module L = Location
open Support

type t =
  { location : L.t
  ; modules : string list
  ; value : string
  }

let make location modules value = { location; modules; value }

let[@inline] location { location; _ } = location

let[@inline] modules { modules; _ } = modules

let[@inline] value { value; _ } = value

include (
  Show.Make (struct
    type nonrec t = t

    let pp ppf n =
      Format.fprintf ppf "%a::%s"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "::")
           (fun ppf x -> Format.fprintf ppf "%s" x))
        (modules n) (value n)
  end) :
    Show.SHOW with type t := t)

include (
  Ord.Make (struct
    type nonrec t = t

    module ModuleListOrd : Ord.ORD with type t = String.t list =
      List.MakeOrd (String)

    let compare x y =
      let comparison = ModuleListOrd.compare (modules x) (modules y) in
      if Stdlib.(comparison <> 0) then comparison
      else String.compare (value x) (value y)
  end) :
    Ord.ORD with type t := t)

include (
  Eq.Make (struct
    type nonrec t = t

    let equal x y =
      List.equal String.equal (modules x) (modules y)
      && String.equal (value x) (value y)
  end) :
    Eq.EQ with type t := t)
