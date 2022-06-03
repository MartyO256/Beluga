(** The abstract datatype of actions. *)
module type MONAD = sig
  (** The type of actions in the monad. *)
  type +'a t

  (** [return a] injects [a] into the monadic type. *)
  val return : 'a -> 'a t

  (** Alias of {!return}. *)
  val pure : 'a -> 'a t

  (** Sequentially composes two actions, passing any value produced by the
      second action as argument to the first. *)
  val bind : ('a -> 'b t) -> 'a t -> 'b t

  (** Operator alias of {!bind}. *)
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val compose : ('b -> 'c t) -> ('a -> 'b t) -> 'a -> 'c t

  val ( >=> ) : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
end

(** Functor building the aliases for a minimal implementation for {!MONAD}. *)
module Make (Monad : sig
  type +'a t

  val return : 'a -> 'a t

  val bind : ('a -> 'b t) -> 'a t -> 'b t
end) : MONAD with type 'a t = 'a Monad.t
