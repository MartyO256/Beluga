open Support

module FullyQualifiedName = struct
  type t =
    { location : Location.t
    ; modules : string list
    ; value : string
    }

  let make location modules value = { location; modules; value }

  let[@inline] location { location; _ } = location

  let[@inline] modules { modules; _ } = modules

  let[@inline] value { value; _ } = value
end

include FullyQualifiedName

include Show.Make (struct
  include FullyQualifiedName

  let pp ppf n =
    Format.fprintf ppf "%a::%s"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf "::")
         (fun ppf x -> Format.fprintf ppf "%s" x))
      (modules n) (value n)
end)

include Eq.Make (struct
  include FullyQualifiedName

  let equal x y =
    List.equal String.equal (modules x) (modules y)
    && String.equal (value x) (value y)
end)

include Ord.Make (struct
  include FullyQualifiedName

  module StringOrd : Ord.ORD with type t = string = Ord.Make (struct
    type t = string

    let compare x y = String.compare x y
  end)

  module ModuleListOrd : Ord.ORD with type t = StringOrd.t list =
    List.MakeOrd (StringOrd)

  let compare x y =
    let comparison = ModuleListOrd.compare (modules x) (modules y) in
    if Stdlib.(comparison <> 0) then comparison
    else String.compare (value x) (value y)
end)
