open Support
open Common
open Internal

type t =
  { id : Id.CompDest.t
  ; name : Name.t
  ; location : Location.t
  ; implicit_arguments : int
  ; mctx : LF.mctx
  ; observation_typ : Comp.typ
  ; return_typ : Comp.typ
  ; kind : Id.CompCotyp.t
  ; documentation_comment : DocumentationComment.t Option.t
  }

let make ~id ~name ~location ~implicit_arguments ~mctx ~observation_typ
    ~return_typ ?documentation_comment kind =
  { id
  ; name
  ; location
  ; implicit_arguments
  ; mctx
  ; observation_typ
  ; return_typ
  ; kind
  ; documentation_comment
  }

let[@inline] id { id; _ } = id

let[@inline] location { location; _ } = location

let[@inline] name { name; _ } = name

let[@inline] implicit_arguments { implicit_arguments; _ } =
  implicit_arguments

let[@inline] mctx { mctx; _ } = mctx

let[@inline] observation_typ { observation_typ; _ } = observation_typ

let[@inline] return_typ { return_typ; _ } = return_typ

let[@inline] kind { kind; _ } = kind

let[@inline] documentation_comment { documentation_comment; _ } =
  documentation_comment
