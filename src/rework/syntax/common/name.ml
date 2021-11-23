open Support

module Name = struct
  type t =
    { location : Location.t
    ; value : string
    }

  let make location value = { location; value }

  let make_blank location = make location "_"

  let location { location; _ } = location

  let value { value; _ } = value
end

include Name

include Show.Make (struct
  include Name

  let pp ppf name = Format.fprintf ppf "%s" name.value
end)

include Eq.Make (struct
  include Name

  let equal x y = String.equal (value x) (value y)
end)

include Ord.Make (struct
  include Name

  let compare x y = String.compare (value x) (value y)
end)
