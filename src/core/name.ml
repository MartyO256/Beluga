open Support

type t =
  { location: Location.t
  ; value: string
  }

let make location value = { location; value }

let location { location; _ } = location


let value { value; _ } = value

include Show.Make (struct
  type tmp = t (* Workaround `type t = t` being recursive *)

  type t = tmp

  let pp ppf name = Format.fprintf ppf "%s" name.value
end)

include Eq.Make (struct
  type tmp = t (* Workaround `type t = t` being recursive *)

  type t = tmp

  let equal x y = String.equal (value x) (value y)
end)

include Ord.Make (struct
  type tmp = t (* Workaround `type t = t` being recursive *)

  type t = tmp

  let compare x y = String.compare (value x) (value y)
end)
