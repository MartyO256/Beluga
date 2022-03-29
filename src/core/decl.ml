open Support

type t = int

let counter () =
  let count = ref 0 in
  fun () ->
    incr count;
    !count

let next = counter ()

include (Ord.Make (Int) : Ord.ORD with type t := t)
