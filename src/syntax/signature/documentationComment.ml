open Support

type t =
  { content : string
  ; location : Location.t
  }

let make ~location content = { content; location }

let[@inline] content { content; _ } = content

let[@inline] location { location; _ } = location
