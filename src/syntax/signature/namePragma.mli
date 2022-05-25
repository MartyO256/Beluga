(** Pragma for modifying the naming conventions of an LF type constant. *)

open Support
open Common

type t

(** {1 Constructor} *)

val make :
     location:Location.t
  -> var_naming_convention:string Option.t
  -> mvar_naming_convention:string
  -> typ:Id.Typ.t
  -> t

(** {1 Destructors} *)

val location : t -> Location.t

val var_naming_convention : t -> string Option.t

val mvar_naming_convention : t -> string

val typ : t -> Id.Typ.t
