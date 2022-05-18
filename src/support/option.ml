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

let get_default default o = value ~default o

let of_bool = function
  | true -> Some ()
  | false -> None

let ( $ ) x f = bind f x

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

type 'a all_or_none =
  [ `all of 'a list
  | `mixed of 'a list
  | `none
  | `empty
  ]

(** Checks whether all or none of the list of options has a value. *)
let rec all_or_none = function
  | None :: xs -> (
    match all_or_none xs with
    | `all xs | `mixed xs -> `mixed xs
    | `none | `empty -> `none)
  | Some x :: xs -> (
    match all_or_none xs with
    | `all xs -> `all (x :: xs)
    | `empty -> `all [ x ]
    | `mixed xs -> `mixed (x :: xs)
    | `none -> `mixed [ x ])
  | [] -> `empty

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
