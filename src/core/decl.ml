open Support

type t = int

let counter () =
  let count = ref 0 in
  fun () ->
    incr count;
    !count

let next = counter ()

module Ord : Ord.ORD with type t := t = Ord.Make (Int)

include Ord
