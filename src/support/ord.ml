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
end) : ORD with type t := T.t = struct
  include T

  let (<) x y = compare x y < 0

  let (<=) x y = compare x y <= 0

  let (>) x y = compare x y > 0

  let (>=) x y = compare x y >= 0

  let min x y =
    if x <= y then x else y

  let max x y =
    if x >= y then x else y
end

module Reverse (Ord : ORD) : ORD with type t := Ord.t = Make (struct
  type t = Ord.t

  let compare x y =
    let comparison = Ord.compare x y in
    if comparison > 0 then -1
    else if comparison = 0 then 0
    else 1
end)
