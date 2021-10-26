open Syntax.Int

let rec numeric_order_arg tau n =
  let open Comp in
  match (tau, n) with
  | (_, 0) -> 0
  | (TypArr (_, _, tau), n) -> 1 + numeric_order_arg tau (n-1)
  | (TypPiBox (_, LF.Decl (_, _, d), tau), n) ->
     let c =
       d
       |> Depend.fold
           ~implicit:(Fun.const 0)
           ~explicit:(Fun.const 1)
           (* We count inductive as 1 instead of throwing an error because
              we would elaboration only works when we don't have
              Inductive. Inductive can however show up later, but we know
              that it couldn't have been on an implicit argument (this is
              forbidden). *)
           ~inductive:(Fun.const 1)
     in
     c + numeric_order_arg tau (n - 1)

let numeric_order tau n = Comp.map_order (numeric_order_arg tau) n
