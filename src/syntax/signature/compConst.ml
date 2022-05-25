open Support
open Common
open Internal

type t =
  { id : Id.CompConst.t
  ; name : Name.t
  ; location : Location.t
  ; implicit_arguments : int
  ; typ : Comp.typ
  ; kind : Id.CompTyp.t
  ; documentation_comment : DocumentationComment.t Option.t
  }

let make ~id ~name ~location ~implicit_arguments ~kind ?documentation_comment
    typ =
  { id
  ; name
  ; location
  ; implicit_arguments
  ; typ
  ; kind
  ; documentation_comment
  }

let[@inline] id { id; _ } = id

let[@inline] location { location; _ } = location

let[@inline] name { name; _ } = name

let[@inline] implicit_arguments { implicit_arguments; _ } =
  implicit_arguments

let[@inline] typ { typ; _ } = typ

let[@inline] kind { kind; _ } = kind

let[@inline] documentation_comment { documentation_comment; _ } =
  documentation_comment
