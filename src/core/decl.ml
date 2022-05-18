open Support
include Int

let counter () =
  let count = ref 0 in
  fun () ->
    incr count;
    !count

let next = counter ()
