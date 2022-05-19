type ('a, 'b) t = 'a * 'b

(** [fst (x, y)] is [x]. *)
val fst : ('a, 'b) t -> 'a

(** [snd (x, y)] is [y]. *)
val snd : ('a, 'b) t -> 'b

(** Transforms the right component of a pair. *)
val rmap : ('a -> 'b) -> 'x * 'a -> 'x * 'b

(** Transforms the left component of a pair. *)
val lmap : ('a -> 'b) -> 'a * 'x -> 'b * 'x

(** Transforms both components of a pair. *)
val bimap : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd

(** Transforms both components of a pair in the same way. *)
val both : ('a -> 'b) -> 'a * 'a -> 'b * 'b

(** Forms a pair from left to right. *)
val left : 'a -> 'b -> 'a * 'b

(** Forms a pair from right to left. *)
val right : 'a -> 'b -> 'b * 'a

(** Swaps a pair. *)
val swap : 'a * 'b -> 'b * 'a

val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c

val equal :
     ('a -> 'a -> bool)
  -> ('b -> 'b -> bool)
  -> ('a, 'b) t
  -> ('a, 'b) t
  -> bool

val compare :
  ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int

val pp :
     (Format.formatter -> 'a -> unit)
  -> (Format.formatter -> 'b -> unit)
  -> Format.formatter
  -> 'a * 'b
  -> unit

val show :
     (Format.formatter -> 'a -> unit)
  -> (Format.formatter -> 'b -> unit)
  -> 'a * 'b
  -> string

(** {1 Instances} *)

module MakeEq (E1 : Eq.EQ) (E2 : Eq.EQ) : Eq.EQ with type t = (E1.t, E2.t) t

module MakeOrd (O1 : Ord.ORD) (O2 : Ord.ORD) :
  Ord.ORD with type t = (O1.t, O2.t) t

module MakeShow (S1 : Show.SHOW) (S2 : Show.SHOW) :
  Show.SHOW with type t = (S1.t, S2.t) t
