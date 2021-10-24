module type EQ = sig
  type t

  val equal : t -> t -> bool

  val (=) : t -> t -> bool

  val (<>) : t -> t -> bool
end

module Make (T : sig
  type t

  val equal : t -> t -> bool
end) : EQ with type t := T.t = struct
  include T

  let (=) = equal

  let (<>) x y = not (x = y)
end
