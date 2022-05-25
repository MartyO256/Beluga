open Support
open Common
open Internal

type search_parameters =
  { expected_solutions : int Option.t
  ; maximum_tries : int Option.t
  ; search_depth : int Option.t
  }

let make_search_parameters ?expected_solutions ?maximum_tries ?search_depth
    () =
  { expected_solutions; maximum_tries; search_depth }

type t =
  { id : Id.Query.t
  ; location : Location.t
  ; name : Name.t Option.t
  ; query : LF.mctx * (LF.typ * offset)
  ; search_parameters : search_parameters
  ; documentation_comment : DocumentationComment.t Option.t
  }

let make ~id ~location ?name ?(search_parameters = make_search_parameters ())
    ?documentation_comment query =
  { id; location; name; search_parameters; query; documentation_comment }

let[@inline] id { id; _ } = id

let[@inline] location { location; _ } = location

let[@inline] name { name; _ } = name

let[@inline] query { query; _ } = query

let[@inline] search_parameters { search_parameters; _ } = search_parameters

let[@inline] documentation_comment { documentation_comment; _ } =
  documentation_comment
