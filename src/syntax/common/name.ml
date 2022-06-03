open Support

type t = string

include (Ord.Make (String) : Ord.ORD with type t := t)

include (
  struct
    let pp = Format.pp_print_string

    let show = Fun.id
  end :
    Show.SHOW with type t := t)

module Set = Set.Make (String)
module Map = Map.Make (String)
module LinkedMap = LinkedMap.Make (Map)
module Hamt = Hamt.Make (String)
module LinkedHamt = Support.LinkedHamt.Make (Hamt)
module LinkedHamt1 = Support.LinkedHamt.Make1 (Hamt)

type fresh_name_supplier = (t -> bool) -> t

exception Fresh_name_generation_error

(** [find names p] is the first name in [names] that satisfies [p].
    The elements in [names] are assumed to be all distinct.

    @raise Fresh_name_generation_error
      If the sequence [names] is fully exhausted without being able to
      generate a fresh name. *)
let rec find : (t -> bool) -> t Seq.t ->  t =
 fun p generate_name ->
  match generate_name () with
  | Seq.Nil -> raise Fresh_name_generation_error
  | Seq.Cons (hd, tl) -> if p hd then hd else find p tl

(** [names_seq base index] is the sequence of names with prefix [base] and
    incremental integer suffix starting with [index].

    For instance, [names_seq "x" 1] is the sequence of names
    [\["x1"; "x2"; ...; "xM"\]] where ["M"] is [Int.max_int]. *)
let rec names_seq : string -> int -> t Seq.t =
 fun base i ->
  Seq.cons
    (base ^ Int.show i)
    (if Int.(i = max_int) then Seq.empty
    else fun () -> names_seq base (i + 1) ())

let prefixed_fresh_name_supplier base =
  let names = Seq.cons base (names_seq base 1) in
  fun p -> find p names
