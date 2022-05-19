include Stdlib.List

let rec last l =
  match l with
  | [] -> raise (Invalid_argument "List.last")
  | [ x ] -> x
  | _ :: xs -> last xs

let rec pairs l =
  match l with
  | [] | [ _ ] -> []
  | x1 :: x2 :: xs -> (x1, x2) :: pairs (x2 :: xs)

let null = function
  | [] -> true
  | _ -> false

let nonempty l = not (null l)

let filter_rev p l =
  let rec go acc = function
    | [] -> acc
    | x :: xs -> go (if p x then x :: acc else acc) xs
  in
  go [] l

let uncons = function
  | [] -> None
  | x :: xs -> Some (x, xs)

let rec concat_map f = function
  | [] -> []
  | x :: xs -> f x @ concat_map f xs

let concat_mapi f =
  let rec go i = function
    | [] -> []
    | x :: xs -> f i x @ go (i + 1) xs
  in
  go 0

let index_of p l =
  let rec go k = function
    | [] -> None
    | x :: _ when p x -> Some k
    | _ :: xs -> go (k + 1) xs
  in
  go 0 l

let find_index p =
  let rec find_index i = function
    | [] -> raise Not_found
    | x :: xs -> if p x then (i, x) else find_index (i + 1) xs
  in
  find_index 0

let find_index_opt p =
  let rec find_index_opt i = function
    | [] -> None
    | x :: xs -> if p x then Some (i, x) else find_index_opt (i + 1) xs
  in
  find_index_opt 0

let rec equal eq l1 l2 =
  match (l1, l2) with
  | [], [] -> true
  | x :: xs, y :: ys when eq x y -> equal eq xs ys
  | _ -> false

let rec compare cmp x y =
  match (x, y) with
  | [], [] -> 0
  | [], _ :: _ -> 1
  | _ :: _, [] -> -1
  | x :: xs, y :: ys ->
    let comparison = cmp x y in
    if comparison = 0 then compare cmp xs ys else comparison

let pp = Format.pp_print_list

let show ?(pp_sep = Format.pp_print_cut) pp_v l =
  Format.asprintf "%a" (pp ~pp_sep pp_v) l

let hd_opt = function
  | [] -> None
  | x :: _ -> Some x

let iter_rev f l =
  let rec iter_rev l continue =
    match l with
    | [] -> continue ()
    | x :: xs ->
      iter_rev xs (fun () ->
          f x;
          continue ())
  in
  iter_rev l (fun () -> ())

let index l = mapi (fun i x -> (i, x)) l

let map f =
  let rec map l return =
    match l with
    | [] -> return []
    | x :: xs ->
      let y = f x in
      map xs (fun ys -> return (y :: ys))
  in
  fun l -> map l Fun.id

let mapi2 f =
  let rec mapi2 index l1 l2 return =
    match (l1, l2) with
    | [], [] -> return []
    | x :: xs, y :: ys ->
      mapi2 (index + 1) xs ys (fun tl -> return (f index x y :: tl))
    | _ -> raise (Invalid_argument "List.mapi2")
  in
  fun l1 l2 -> mapi2 0 l1 l2 Fun.id

let rec drop n = function
  | l when n <= 0 -> l
  | [] -> []
  | _ :: xs (* n > 0 *) -> drop (n - 1) xs

let ap xs = map2 (fun x f -> f x) xs

let ap_one x = map (fun f -> f x)

let split =
  let rec split l return =
    match l with
    | [] -> return ([], [])
    | (x, y) :: l -> split l (fun (xs, ys) -> return (x :: xs, y :: ys))
  in
  fun l -> split l Fun.id

let combine =
  let rec combine l1 l2 return =
    match (l1, l2) with
    | [], [] -> return []
    | a1 :: l1, a2 :: l2 ->
      combine l1 l2 (fun rest -> return ((a1, a2) :: rest))
    | _ -> raise (Invalid_argument "List.combine")
  in
  fun l1 l2 -> combine l1 l2 Fun.id

let partitioni p l =
  let rec partitioni i yes no = function
    | [] -> (rev yes, rev no)
    | x :: l ->
      if p i x then partitioni (i + 1) (x :: yes) no l
      else partitioni (i + 1) yes (x :: no) l
  in
  partitioni 0 [] [] l

let fold_right f l acc =
  let rec fold_right l acc return =
    match l with
    | [] -> return acc
    | a :: l -> fold_right l acc (fun b -> return (f a b))
  in
  fold_right l acc Fun.id

let partition_take k l =
  let rec partition_take k l taken =
    match l with
    | x :: xs when k > 0 -> partition_take (k - 1) xs (x :: taken)
    | _ -> (rev taken, l)
  in
  partition_take k l []

module MakeEq (E : Eq.EQ) : Eq.EQ with type t = E.t t = Eq.Make (struct
  type nonrec t = E.t t

  let equal = equal E.equal
end)

module MakeOrd (O : Ord.ORD) : Ord.ORD with type t = O.t t = Ord.Make (struct
  type nonrec t = O.t t

  let compare = compare O.compare
end)

module MakeShow (S : Show.SHOW) : Show.SHOW with type t = S.t t =
Show.Make (struct
  type nonrec t = S.t t

  let pp = pp S.pp
end)
