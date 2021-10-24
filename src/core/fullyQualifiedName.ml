open Support

type t =
  { location: Location.t
  ; modules: string list
  ; value: string
  }

let make location modules value = { location; modules; value }

let location { location; _ } = location

let modules { modules; _ } = modules

let value { value; _ } = value

module Show = Show.Make (struct
  type tmp = t (* Workaround `type t = t` being recursive *)

  type t = tmp

  let pp ppf n =
    Format.fprintf ppf "%a::%s"
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf "::")
        (fun ppf x -> Format.fprintf ppf "%s" x)
      )
      (modules n)
      (value n)
end)

module Eq = Eq.Make (struct
  type tmp = t (* Workaround `type t = t` being recursive *)

  type t = tmp

  let equal x y =
    List.equal String.equal (modules x) (modules y)
    && String.equal (value x) (value y)
end)

module Ord = Ord.Make (struct
  type tmp = t (* Workaround `type t = t` being recursive *)

  type t = tmp

  module StringOrd : Ord.ORD with type t = string = Ord.Make (struct
    type t = string

    let compare x y = String.compare x y
  end)

  module ModuleListOrd : Ord.ORD with type t = StringOrd.t list = List.MakeOrd (StringOrd)

  let compare x y =
    let comparison = ModuleListOrd.compare (modules x) (modules y) in
    if Stdlib.(comparison <> 0) then comparison
    else String.compare (value x) (value y)
end)
