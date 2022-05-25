open Support
open Common
open Internal

type t =
  { id : Id.Comp.t
  ; name : Name.t
  ; location : Location.t
  ; implicit_arguments : int
  ; typ : Comp.typ
  ; mutual_group : Id.Comp.t List1.t Option.t
  ; program : Comp.value
  ; documentation_comment : DocumentationComment.t Option.t
  }

let make ~id ~name ~location ~implicit_arguments ~typ ?mutual_group
    ?documentation_comment program =
  { id
  ; name
  ; location
  ; implicit_arguments
  ; typ
  ; mutual_group
  ; program
  ; documentation_comment
  }

let[@inline] id { id; _ } = id

let[@inline] location { location; _ } = location

let[@inline] name { name; _ } = name

let[@inline] implicit_arguments { implicit_arguments; _ } =
  implicit_arguments

let[@inline] typ { typ; _ } = typ

let[@inline] program { program; _ } = program

let[@inline] mutual_group { mutual_group; _ } = mutual_group

let[@inline] documentation_comment { documentation_comment; _ } =
  documentation_comment
