include Stdlib.Option

let eliminate (def : unit -> 'b) (f : 'a -> 'b) : 'a option -> 'b = function
  | None -> def ()
  | Some x -> f x

(** Extracts the value from an option, throwing an exception if there's None. *)
let get' (e : exn) (o : 'a option) : 'a = eliminate (Misc.throw e) Fun.id o

let get_or_else default = function
  | None -> default ()
  | Some x -> x

let get_default def o = eliminate (Fun.const def) Fun.id o

let of_bool = function
  | true -> Some ()
  | false -> None

let ( $ ) = bind

let ( >>= ) = bind

let flat_map k o = o $ k

(** Prioritized choice between options. *)
let lazy_alt (p : 'a option Lazy.t) (q : 'a option Lazy.t) : 'a option Lazy.t
  =
  lazy
    (let p = Lazy.force p in
     match p with
     | Some _ -> p
     | None -> Lazy.force q)

(* This is hoisted out so that forcing becomes a no-op after the first force. *)
let lazy_none = lazy None

let choice (ps : 'a option Lazy.t list) : 'a option Lazy.t =
  List.fold_left lazy_alt lazy_none ps

(** Non-lazy version of `<|>'. *)
let alt (o1 : 'a option) (o2 : 'a option) : 'a option =
  match o1, o2 with
  | Some x, _ -> Some x
  | _, Some y -> Some y
  | _ -> None

let rec traverse (f : 'a -> 'b option) (xs : 'a list) : 'b list option =
  match xs with
  | [] -> Some []
  | x :: xs -> f x $ fun y -> traverse f xs $ fun ys -> Some (y :: ys)

let rec traverse_ (f : 'a -> unit option) (xs : 'a list) : unit option =
  match xs with
  | [] -> Some ()
  | x :: xs -> f x $ fun _ -> traverse_ f xs

let rec fold_left (f : 'b -> 'a -> 'b option) (acc : 'b) (xs : 'a list)
    : 'b option
  =
  match xs with
  | [] -> Some acc
  | x :: xs -> f acc x $ fun acc' -> fold_left f acc' xs

let ( $> ) (o : 'a option) (f : 'a -> 'b) : 'b option =
  o $ fun x -> Some (f x)

(** Ignores the result of the first option and gives the second. *)
let ( &> ) (o : 'a option) (o' : 'b option) : 'b option = o $ fun _ -> o'

let void (o : 'a option) : unit option = o $> fun _ -> ()
let cat_options (l : 'a option list) : 'a list = List.filter_map Fun.id l

(** Specialized effectful eliminator for option types. *)
let when_some (l : 'a option) (f : 'a -> unit) : unit =
  eliminate (fun _ -> ()) f l

type 'a all_or_none =
  [ `all of 'a list
  | `mixed of 'a list
  | `none
  | `empty
  ]

(** Checks whether all or none of the list of options has a value. *)
let rec all_or_none = function
  | None :: xs ->
    (match all_or_none xs with
    | `all xs
    | `mixed xs ->
      `mixed xs
    | `none
    | `empty ->
      `none)
  | Some x :: xs ->
    (match all_or_none xs with
    | `all xs -> `all (x :: xs)
    | `empty -> `all [ x ]
    | `mixed xs -> `mixed (x :: xs)
    | `none -> `mixed [ x ])
  | [] -> `empty

(** Eliminate the option into a printer. *)
let print'
    (none : Format.formatter -> unit -> unit)
    (some : Format.formatter -> 'a -> unit)
    (ppf : Format.formatter)
    (m : 'a option)
    : unit
  =
  eliminate (none ppf) (some ppf) m

(** Eliminate the option into a printer, emitting nothing if there's nothing
    there. *)
let print
    (f : Format.formatter -> 'a -> unit)
    (ppf : Format.formatter)
    (m : 'a option)
    : unit
  =
  print' (fun _ _ -> ()) f ppf m

(** Print an option for debugging. *)
let show (f : Format.formatter -> 'a -> unit) (ppf : Format.formatter)
    : 'a option -> unit
  =
  let open Format in
  eliminate
    (fun () -> fprintf ppf "None")
    (fun x -> fprintf ppf "Some (@[%a@])" f x)
