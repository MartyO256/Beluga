open Support

type t = int

let decl_counter = ref 0

let next () =
  incr decl_counter;
  !decl_counter

let reset () = decl_counter := 0

module Ord : Ord.ORD with type t := t = Ord.Make (Int)

include Ord
