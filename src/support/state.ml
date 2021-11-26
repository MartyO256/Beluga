module type STATE = sig
  type state

  include Monad.MONAD

  val get : state t

  val put : state -> unit t

  val run : 'a t -> init:state -> state * 'a
end

module Make (State : sig
  type t
end) : STATE with type state = State.t = struct
  type state = State.t

  include Monad.Make (struct
    (** The type of state transformers. *)
    type 'a t = state -> state * 'a

    let return a s = (s, a)

    let bind f v s =
      let s', a = v s in
      f a s'
  end)

  let get s = (s, s)

  let put s' _ = (s', ())

  let run m ~init = m init
end
