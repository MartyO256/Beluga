module L = Location
open Support

type t =
  { location : L.t
  ; value : string
  }

let make location value = { location; value }

let make_blank location = make location "_"

let[@inline] location { location; _ } = location

let[@inline] value { value; _ } = value

module OrdByValue = (val Ord.contramap (module String) value)

include (OrdByValue : Ord.ORD with type t := t)

module ShowByValue = (val Show.contramap (module String) value)

include (ShowByValue : Show.SHOW with type t := t)
