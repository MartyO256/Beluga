include Stdlib.Result

let of_bool b err = if b then ok () else error @@ err ()

let ( >>= ) = bind

let ( $> ) x y = map y x

let get_or_else f = fold ~ok:Fun.id ~error:f
