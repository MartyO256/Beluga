module type STATE = sig
  type state

  include Monad.MONAD

  val get : state t

  val put : state -> unit t

  val run : 'a t -> init:state -> state * 'a

  include Functor.FUNCTOR with type 'a t := 'a t

  include Apply.APPLY with type 'a t := 'a t
end

module Make (S : sig
  type t
end) : STATE with type state = S.t = struct
  module State = struct
    type state = S.t

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

  include State
  include Functor.Make (State)
  include Apply.Make (State)
end
