module type ORD = sig
  type t

  val compare : t -> t -> int
  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val max : t -> t -> t
  val min : t -> t -> t
end

module Make (T : sig
  type t

  val compare : t -> t -> int
end) : ORD with type t = T.t = struct
  include T

  let[@inline] ( = ) x y = compare x y = 0
  let[@inline] ( <> ) x y = compare x y <> 0
  let[@inline] ( < ) x y = compare x y < 0
  let[@inline] ( <= ) x y = compare x y <= 0
  let[@inline] ( > ) x y = compare x y > 0
  let[@inline] ( >= ) x y = compare x y >= 0
  let[@inline] min x y = if x <= y then x else y
  let[@inline] max x y = if x >= y then x else y
end

module Reverse (Ord : ORD) : ORD with type t = Ord.t = Make (struct
  type t = Ord.t

  let compare x y = -Ord.compare x y
end)
