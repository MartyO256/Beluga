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

type fresh_name_supplier = Set.t -> t

(** [find_distinct names used_names] is the first name in [names] that is not
    a member of [used_names]. The elements in [names] are assumed to be all
    distinct.

    @raise Invalid_argument
      if the sequence [names] is fully exhausted without being able to
      generate a fresh name. *)
let rec find_distinct : t Seq.t -> Set.t -> t =
 fun generate_name used_names ->
  match generate_name () with
  | Seq.Nil -> raise @@ Invalid_argument "Exhausted sequence of fresh names"
  | Seq.Cons (hd, tl) ->
    if Set.mem hd used_names then find_distinct tl used_names else hd

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
  find_distinct @@ Seq.cons base (names_seq base 1)
