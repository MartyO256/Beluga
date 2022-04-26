(** Linked association tables over ordered types.

    These are finite maps that keep track the sequence of unique bindings
    added to it. This allows for tracing of the additions to the map. *)
module type S = sig
  (** The type of the map keys. *)
  type key

  (** The type of linked maps from type {!type:key} to type ['a]. *)
  type 'a t

  (** {1 Constructors} *)

  (** The empty linked map. *)
  val empty : 'a t

  (** [add key data m] returns a linked map containing the same bindings as
      [m], plus a binding of [key] to [data]. If [key] was already bound in
      [m] to a value, then [m] is returned. *)
  val add : key -> 'a -> 'a t -> 'a t

  (** [singleton key data] returns the one-element map that contains a
      binding [data] for [key]. *)
  val singleton : key -> 'a -> 'a t

  (** {1 Predicates} *)

  (** [is_empty m] returns [true] if [m] has no bindings. *)
  val is_empty : 'a t -> bool

  (** [mem x m] returns [true] if [m] contains a binding for [x], and [false]
      otherwise. *)
  val mem : key -> 'a t -> bool

  (** {1 Iterators} *)

  (** [iter f m] applies [f] to all the additions to [m] made using
      {!val:add}. This includes bindings that were replaced in the map. [f]
      receives the key as first argument, and the associated value as second
      argument. *)
  val iter : (key -> 'a -> unit) -> 'a t -> unit

  (** [iter_bindings f m] applies [f] to all the current bindings in map [m].
      [f] receives the key as first argument, and the associated value as
      second argument. *)
  val iter_bindings : (key -> 'a -> unit) -> 'a t -> unit

  (** [fold_left f m init] computes
      [f (... (f (f init k1 d1) k2 d2) ...) km dm] where [k1 ... km] are all
      the keys of all additions to [m], and [d1 ... dm] are the associated
      data. Note that some of the traversed bindings may not be part of the
      current bindings in the map. *)
  val fold_left : ('a -> key -> 'b -> 'a) -> 'b t -> 'a -> 'a

  (** [fold_right f m init] computes
      [f k1 d1 (f k2 d2 (... (f km dm init) ...))] where [k1 ... km] are all
      the keys of all additions to [m], and [d1 ... dm] are the associated
      data. Note that some of the traversed bindings may not be part of the
      current bindings in the map. *)
  val fold_right : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (** [fold_bindings f m init] computes [(f kN dN ... (f k1 d1 init)...)],
      where [k1 ... kN] are the keys of all bindings in [m], and [d1 ... dN]
      are the associated data. *)
  val fold_bindings : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (** {1 Scanning} *)

  (** [exists p m] checks if at least one of the current bindings of [m]
      satisfies the predicate [p]. [p] receives the key as first argument,
      and the associated value as second argument. *)
  val exists : (key -> 'a -> bool) -> 'a t -> bool

  (** [for_all p m] checks if all of the current bindings of [m] satisfies
      the predicate [p]. [p] receives the key as first argument, and the
      associated value as second argument. *)
  val for_all : (key -> 'a -> bool) -> 'a t -> bool

  (** [find_opt key m] returns [Some v] if the current value of [key] in [m]
      is [v], or [None] if no binding for [key] exists. *)
  val find_opt : key -> 'a t -> 'a option
end

(** Functor building an implementation of the linked map structure given a
    map structure over ordered types. *)
module Make (Map : Map.S) : S with type key = Map.key

(** Non-empty linked association tables over ordered types.

    These are finite maps that keep track the sequence of bindings added to
    it. This allows for tracing of the additions to the map. *)
module type S1 = sig
  (** The type of the map keys. *)
  type key

  (** The type of linked maps from type {!type:key} to type ['a]. *)
  type 'a t

  (** {1 Constructors} *)

  (** [singleton key data] returns the one-element map that contains a
      binding [data] for [key]. *)
  val singleton : key -> 'a -> 'a t

  (** [add key data m] returns a linked map containing the same bindings as
      [m], plus a binding of [key] to [data]. If [key] was already bound in
      [m] to a value, then [m] is returned. *)
  val add : key -> 'a -> 'a t -> 'a t

  (** {1 Predicates} *)

  (** [mem x m] returns [true] if [m] contains a binding for [x], and [false]
      otherwise. *)
  val mem : key -> 'a t -> bool

  (** {1 Iterators} *)

  (** [iter f m] applies [f] to all the additions to [m] made using
      {!val:add}. This includes bindings that were replaced in the map. [f]
      receives the key as first argument, and the associated value as second
      argument. *)
  val iter : (key -> 'a -> unit) -> 'a t -> unit

  (** [iter_bindings f m] applies [f] to all the current bindings in map [m].
      [f] receives the key as first argument, and the associated value as
      second argument. *)
  val iter_bindings : (key -> 'a -> unit) -> 'a t -> unit

  (** [fold_left f g m init] computes [g (... (g (f k1 d1) k2 d2) ...) km dm]
      where [k1 ... km] are all the keys of all additions to [m], and
      [d1 ... dm] are the associated data. Note that some of the traversed
      bindings may not be part of the current bindings in the map. *)
  val fold_left : (key -> 'b -> 'a) -> ('a -> key -> 'b -> 'a) -> 'b t -> 'a

  (** [fold_right f g m init] computes
      [g k1 d1 (g k2 d2 (... (f km dm) ...))] where [k1 ... km] are all the
      keys of all additions to [m], and [d1 ... dm] are the associated data.
      Note that some of the traversed bindings may not be part of the current
      bindings in the map. *)
  val fold_right : (key -> 'a -> 'b) -> (key -> 'a -> 'b -> 'b) -> 'a t -> 'b

  (** [fold_bindings f g m init] computes [(g kN dN ... (f k1 d1)...)], where
      [k1 ... kN] are the keys of all bindings in [m], and [d1 ... dN] are
      the associated data. *)
  val fold_bindings :
    (key -> 'a -> 'b) -> (key -> 'a -> 'b -> 'b) -> 'a t -> 'b

  (** {1 Scanning} *)

  (** [exists p m] checks if at least one of the current bindings of [m]
      satisfies the predicate [p]. [p] receives the key as first argument,
      and the associated value as second argument. *)
  val exists : (key -> 'a -> bool) -> 'a t -> bool

  (** [for_all p m] checks if all of the current bindings of [m] satisfies
      the predicate [p]. [p] receives the key as first argument, and the
      associated value as second argument. *)
  val for_all : (key -> 'a -> bool) -> 'a t -> bool

  (** [find_opt key m] returns [Some v] if the current value of [key] in [m]
      is [v], or [None] if no binding for [key] exists. *)
  val find_opt : key -> 'a t -> 'a option
end

(** Functor building an implementation of the non-empty linked map structure
    given a map structure over ordered types. *)
module Make1 (Map : Map.S) : S1 with type key = Map.key
