include Stdlib.Int

include (Ord.Make (Stdlib.Int) : Ord.ORD with type t := t)
