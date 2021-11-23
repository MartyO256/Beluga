(** The type of non-empty lists. *)
type 'a t

(** Type alias for {!t}. *)
type 'a nonempty = 'a t

(** [uncons l] is [(head l, tail l)]. *)
val uncons : 'a t -> 'a * 'a list

(** [head l] is the first element in [l]. *)
val head : 'a t -> 'a

(** [tail l] is the list of elements that succeed the head of [l]. *)
val tail : 'a t -> 'a list

(** [unsnoc (a_1, \[a_2; ...; a_(n-1); a_n\])] is
    [(\[a_1; a_2; ...; a_(n-1)\], a_n)], with [a_n] being the last element in
    the non-empty list, and [\[a_1; a_2; ...; a_(n-1)\]] being the list of
    elements that precede [a_n] in order. *)
val unsnoc : 'a t -> 'a list * 'a

(** [last l] is the last element in [l]. *)
val last : 'a t -> 'a

(** Constructs a nonempty list given an element. *)
val from : 'a -> 'a list -> 'a t

(** [singleton e] is the non-empty list with the single element [e] in it. *)
val singleton : 'a -> 'a t

(** [fold_right sing cons (a1, \[a2; ...; an\])] is
    [cons a1 (cons a2 (... (sing an) ...))]. *)
val fold_right
  :  ('a -> 'b)
  -> ('a -> 'b -> 'b)
  -> 'a t
  -> 'b

(** [fold_left sing cons (a1, \[a2; ...; an\])] is
    [cons (... (cons (sing a1) a2) ....) an]. *)
val fold_left
  :  ('a -> 'b)
  -> ('b -> 'a -> 'b)
  -> 'a t
  -> 'b

(** [destructure f l] is [f h t], where [h] and [t] are the head and tail of
    [l] respectively. *)
val destructure : ('a -> 'a list -> 'b) -> 'a t -> 'b

(** Counts the number of elements in the nonempty list. *)
val length : 'a t -> int

(** Converts a list to a nonempty list. *)
val of_list : 'a list -> 'a t option

(** Converts a nonempty list to a list. *)
val to_list : 'a t -> 'a list

(** Maps a function over the nonempty list. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** [map2 f (a1, \[a2; ...; an\]) (b1, \[b2; ...; bn\])] is
    [(f a1 b1, \[f a2 b2; ...; f an bn\])].

    @raise Invalid_argument if the two lists are determined to have different
    lengths. *)
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

(** [filter_map f l] applies [f] to every element of [l], filters out the
    [None] elements and returns the list of the arguments of the [Some]
    elements. *)
val filter_map : ('a -> 'b option) -> 'a t -> 'b list

(** Runs an effectful function over the nonempty list. *)
val iter : ('a -> unit) -> 'a t -> unit

val for_all : ('a -> bool) -> 'a t -> bool

(** Collapses a nonempty sequence to a single element, provided all elements
    are (structurally) equal. *)
val all_equal : 'a t -> 'a option

(** Finds the leftmost minimal element of the sequence according to the given
    decision procedure for the strict less-than relation on 'a. *)
val minimum_by : ('a -> 'a -> bool) -> 'a t -> 'a

(** Finds the leftmost minimal element of the sequence according to the
    default ordering for the type 'a. *)
val minimum : 'a t -> 'a

(** Finds the leftmost maximal element of the sequence according to the
    default ordering for the type 'a. *)
val maximum : 'a t -> 'a

(** Groups elements of a list according to a key computed from the element.
    The input list need not be sorted. (The algorithm inserts every element
    of the list into a hashtable in order to construct the grouping.) Each
    group is guaranteed to be nonempty. *)
val group_by : ('a -> 'key) -> 'a list -> ('key * 'a t) list

val print
  :  ?pp_sep:(Format.formatter -> unit -> unit)
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a t
  -> unit

(** Transform a non-empty list of pairs into a pair of non-empty lists:
    [split ((a1, b1), \[(a2, b2); ...; (an, bn)\])] is
    [(a1, \[a2; ...; an\]), (b1, \[b2; ...; bn\])]. *)
val split : ('a * 'b) t -> 'a t * 'b t

(** Transform a pair of lists into a list of pairs:
    [combine (a1, \[a2; ...; an\]) (b1, \[b2; ...; bn\])] is
    [((a1, b1), \[(a2, b2); ...; (an, bn)\])].

    @raise Invalid_argument if the two lists have different lengths. *)
val combine : 'a t -> 'b t -> ('a * 'b) t

(** [partition f l] returns a pair of lists [(l1, l2)], where [l1] is the
    list of all the elements of [l] that satisfy the predicate [f], and [l2]
    is the list of all the elements of [l] that do not satisfy [f]. The order
    of elements in the input list is preserved. At least one of [l1] and [l2]
    is non-empty. *)
val partition : ('a -> bool) -> 'a t -> 'a list * 'a list

(** [ap (x1, \[x2; ...; xn\]) (f1, \[f2; ...; fn\])] is
    [(f1 x1, \[f2 x2; ...; fn xn\])].

    @raise Invalid_argument if the two lists are determined to have different
    lengths. *)
val ap : 'a t -> ('a -> 'b) t -> 'b t

(** [ap_one x (f1, \[f2; ...; fn\])] is [(f1 x, \[f2 x; ...; fn x\])]. *)
val ap_one : 'a -> ('a -> 'b) t -> 'b t

module Syntax : sig
  val ( $> ) : 'a t -> ('a -> 'b) -> 'b t
end

(** [pp ?pp_sep pp_v ppf l] prints the items of the non-empty list [l] using
    [pp_v] to print each item and calling [pp_sep] between items. *)
val pp
  :  ?pp_sep:(Format.formatter -> unit -> unit)
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a t
  -> unit
