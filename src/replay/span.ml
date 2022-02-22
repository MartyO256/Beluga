open Support
open Loc
type loc = Loc.t

type t =
  { start : loc;
    stop : loc;
  }

let of_pair (start : loc) (stop : loc) : t option =
  if compare start stop <= 0 then
    Some { start = start; stop = stop }
  else
    None

exception InvalidSpan
let of_pair' (start : loc) (stop : loc) : t =
  of_pair start stop |>
    Option.eliminate
      (fun () -> raise InvalidSpan)
      (fun x -> x)

let to_string (s : t) : string =
  let open Printf in
  if line s.start = line s.stop then
    sprintf "line %d column %d-%d" (line s.start) (column s.start) (column s.stop)
  else
    sprintf "line %d column %d to line %d column %d"
      (line s.start) (column s.start) (line s.stop) (column s.stop)
