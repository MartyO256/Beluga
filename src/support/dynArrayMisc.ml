include DynArray

(** [append_list dst l] effectfully appends all the elements of [l] to [dst]. *)
let rec append_list d = function
  | [] -> ()
  | x :: xs ->
    add d x;
    append_list d xs

(** [head d] is [Some h] with [h] being the first element of [d] if [d] is
    non-empty, and [None] otherwise. *)
let head = function
  | d when empty d -> None
  | d -> Some (get d 0)

(** [get_opt d i] is [Some (get d i)] if [d] has an element at index [i], and
    [None] otherwise. *)
let get_opt d i = try Some (get d i) with Invalid_arg _ -> None

(** [rfind_opt_idx d p] is [Some (i, l)] where [l] is the last element in [d]
    that satisfies [p] and [i] is the index of [l] in [d], and [None]
    otherwise. *)
let rfind_opt_idx d p =
  let rec go = function
    | -1 -> None
    | k ->
      let x = get d k in
      if p x then Some (k, x) else go (k - 1)
  in
  go (length d - 1)
