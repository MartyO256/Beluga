include Stdlib.Option

module M = Monad.Make (struct
  type nonrec 'a t = 'a t

  let return = some

  let bind f x = bind x f
end)

include (M : Monad.MONAD with type 'a t := 'a t)

include (Functor.Make (M) : Functor.FUNCTOR with type 'a t := 'a t)

include (Apply.Make (M) : Apply.APPLY with type 'a t := 'a t)

let eliminate def f = function
  | None -> def ()
  | Some x -> f x

(** Extracts the value from an option, throwing an exception if there's None. *)
let get' e o = eliminate (Misc.throw e) Fun.id o

let get_or_else default = eliminate default Fun.id

let of_bool = function
  | true -> Some ()
  | false -> None

let from_predicate p a = if p a then some a else none

let flat_map k o = o >>= k

(** Prioritized choice between options. *)
let lazy_alt p q =
  lazy
    (let p = Lazy.force p in
     match p with
     | Some _ -> p
     | None -> Lazy.force q)

(* This is hoisted out so that forcing becomes a no-op after the first
   force. *)
let lazy_none = lazy None

let choice ps = List.fold_left lazy_alt lazy_none ps

(** Non-lazy version of `<|>'. *)
let alt o1 o2 =
  match (o1, o2) with
  | Some x, _ -> Some x
  | _, Some y -> Some y
  | _ -> None

let rec traverse f xs =
  match xs with
  | [] -> Some []
  | x :: xs ->
    f x >>= fun y ->
    traverse f xs >>= fun ys -> Some (y :: ys)

let rec traverse_ f xs =
  match xs with
  | [] -> Some ()
  | x :: xs -> f x >>= fun _ -> traverse_ f xs

let rec fold_left f acc xs =
  match xs with
  | [] -> Some acc
  | x :: xs -> f acc x >>= fun acc' -> fold_left f acc' xs

let void o = o $> Fun.const ()

let cat_options l = List.filter_map Fun.id l

(** Specialized effectful eliminator for option types. *)
let when_some l f = eliminate (Fun.const ()) f l

(** Eliminate the option into a printer. *)
let print' none some ppf m = eliminate (none ppf) (some ppf) m

(** Eliminate the option into a printer, emitting nothing if there's nothing
    there. *)
let print f ppf m = print' (fun _ _ -> ()) f ppf m

(** Print an option for debugging. *)
let show f ppf =
  let open Format in
  eliminate
    (fun () -> fprintf ppf "None")
    (fun x -> fprintf ppf "Some (@[%a@])" f x)

module MakeEq (E : Eq.EQ) : Eq.EQ with type t = E.t t = Eq.Make (struct
  type nonrec t = E.t t

  let equal x y =
    match (x, y) with
    | None, None -> true
    | Some x, Some y when x == y -> true (* Optimization *)
    | Some x, Some y -> E.equal x y
    | _ -> false
end)

module MakeOrd (O : Ord.ORD) : Ord.ORD with type t = O.t t = Ord.Make (struct
  type nonrec t = O.t t

  let compare x y =
    match (x, y) with
    | None, None -> 0
    | Some x, Some y when x == y -> 0 (* Optimization *)
    | Some x, Some y -> O.compare x y
    | None, Some _ -> -1
    | Some _, None -> 1
end)
