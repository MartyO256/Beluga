module type SHOW = sig
  type t
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Make (T : sig
  type t
  val pp : Format.formatter -> t -> unit
end) : SHOW with type t := T.t
