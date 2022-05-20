include Stdlib.Int

module type ORD = Ord.ORD

module Ord : ORD with type t = t = Ord.Make (Stdlib.Int)

include (Ord : ORD with type t := t)

module type HASH = Hash.HASH

module Hash : HASH with type t = t = Hash.Make (Stdlib.Int)

include (Hash : HASH with type t := t)

include (
  Show.Make (struct
    type nonrec t = t

    let pp = Format.pp_print_int
  end) :
    Show.SHOW with type t := t)

module Set = Set.Make (Ord)
module Map = Map.Make (Ord)

module Hamt = HamtMisc.Make (struct
  include Ord
  include Hash
end)