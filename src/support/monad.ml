module type MONAD = sig
  type +'a t

  val return : 'a -> 'a t

  val pure : 'a -> 'a t

  val bind : ('a -> 'b t) -> 'a t -> 'b t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Make (Monad : sig
  type +'a t

  val return : 'a -> 'a t

  val bind : ('a -> 'b t) -> 'a t -> 'b t
end) =
struct
  include Monad

  let pure = return

  let[@inline] ( >>= ) a f = bind f a
end
