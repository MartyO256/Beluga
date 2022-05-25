open Support
open Common
open Internal

type t =
  { id : Id.Schema.t
  ; name : Name.t
  ; location : Location.t
  ; schema : LF.schema
  ; documentation_comment : DocumentationComment.t Option.t
  }

let make ~id ~name ~location ?documentation_comment schema =
  { id; name; location; schema; documentation_comment }

let[@inline] id { id; _ } = id

let[@inline] name { name; _ } = name

let[@inline] location { location; _ } = location

let[@inline] schema { schema; _ } = schema

let[@inline] documentation_comment { documentation_comment; _ } =
  documentation_comment
