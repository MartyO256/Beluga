module type ORD = sig
  type t

  val compare : t -> t -> int

  val (<) : t -> t -> bool

  val (<=) : t -> t -> bool

  val (>) : t -> t -> bool

  val (>=) : t -> t -> bool

  val max : t -> t -> t

  val min : t -> t -> t
end

module Make (T : sig
  type t

  val compare : t -> t -> int
end) : ORD with type t := T.t

module Reverse (Ord: ORD) : ORD with type t := Ord.t
