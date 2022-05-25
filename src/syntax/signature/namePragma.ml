open Support
open Common

type t =
  { location : Location.t
  ; var_naming_convention : string Option.t
  ; mvar_naming_convention : string
  ; typ : Id.Typ.t
  }

let make ~location ~var_naming_convention ~mvar_naming_convention ~typ =
  { location; var_naming_convention; mvar_naming_convention; typ }

let[@inline] location { location; _ } = location

let[@inline] var_naming_convention { var_naming_convention; _ } =
  var_naming_convention

let[@inline] mvar_naming_convention { mvar_naming_convention; _ } =
  mvar_naming_convention

let[@inline] typ { typ; _ } = typ
