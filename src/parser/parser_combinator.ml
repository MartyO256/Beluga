open Support
open Beluga_syntax

module type PARSER_STATE = sig
  type t

  type token

  val observe : t -> (token * t) option
end

module type PARSER_BACKTRACKING_STATE = sig
  type t

  val enable_backtracking : t -> t

  val disable_backtracking : t -> t

  val can_backtrack : from:t -> to_:t -> bool
end

module type PARSER_LOCATION_STATE = sig
  type t

  type location

  val next_location : t -> location option

  val previous_location : t -> location option
end

module type PARSER = sig
  type token

  type input

  type state

  type 'a t = state -> state * ('a, exn) result

  type 'a parser = 'a t

  exception Parser_error of exn

  exception
    Labelled_exception of
      { label : string
      ; cause : exn
      }

  exception No_more_choices of exn list

  exception Expected_end_of_input

  include Monad.MONAD with type 'a t := 'a t

  include Functor.FUNCTOR with type 'a t := 'a t

  include Apply.APPLY with type 'a t := 'a t

  val get_state : state t

  val put_state : state -> unit t

  val run : 'a t -> state -> state * ('a, exn) result

  val run_exn : 'a t -> state -> state * 'a

  val catch :
    'a t -> (state * ('a, exn) result -> state * ('b, exn) result) -> 'b t

  val fail : exn -> 'a t

  val labelled : string -> 'a t -> 'a t

  val only : 'a t -> 'a t

  val maybe : 'a t -> 'a option t

  val maybe_default : 'a t -> default:'a -> 'a t

  val void : 'a t -> unit t

  val many : 'a t -> 'a list t

  val some : 'a t -> 'a List1.t t

  val sep_by0 : 'a t -> unit t -> 'a list t

  val sep_by1 : 'a t -> unit t -> 'a List1.t t

  val traverse_list : ('a -> 'b t) -> 'a list -> 'b list t

  val traverse_list_void : ('a -> unit t) -> 'a list -> unit t

  val seq_list : 'a t list -> 'a list t

  val trying : 'a t -> 'a t

  val alt : 'a t -> 'a t -> 'a t

  val choice : 'a t List.t -> 'a t

  val satisfy :
       on_token:(token -> ('a, exn) result)
    -> on_end_of_input:(unit -> 'a t)
    -> 'a t

  val eoi : unit t
end

module type PARSER_WITH_LOCATIONS = sig
  include PARSER

  type location

  val span : 'a t -> (location * 'a) t

  val fail_at_next_location : exn -> 'a t

  val fail_at_previous_location : exn -> 'a t
end

module Make_persistent_bracktracking_state (Token : sig
  type t
end) : sig
  include PARSER_STATE with type token = Token.t

  include PARSER_BACKTRACKING_STATE with type t := t

  val initial : token Seq.t -> t
end = struct
  type token = Token.t

  type t =
    { input : token Seq.t
    ; position : int
    ; can_backtrack : bool
    }

  let initial input = { input; position = 0; can_backtrack = false }

  let[@inline] position s = s.position

  let[@inline] enable_backtracking s = { s with can_backtrack = true }

  let[@inline] disable_backtracking s = { s with can_backtrack = false }

  let observe s =
    match s.input () with
    | Seq.Nil -> Option.none
    | Seq.Cons (x, xs) ->
        let s' = { s with input = xs; position = s.position + 1 } in
        Option.some (x, s')

  let[@inline] has_not_consumed_input ~from ~to_ =
    position from = position to_

  let can_backtrack ~from ~to_ =
    if from.can_backtrack then true else has_not_consumed_input ~from ~to_
end

module Make_location_state (Token : sig
  type t

  val location : t -> Location.t
end) (State : sig
  include PARSER_STATE with type token = Token.t

  include PARSER_BACKTRACKING_STATE with type t := t
end) : sig
  include module type of State

  include
    PARSER_LOCATION_STATE with type t := t and type location = Location.t

  val initial : ?last_location:Location.t -> State.t -> t
end = struct
  type location = Location.t

  type token = Token.t

  type t =
    { inner_state : State.t
    ; last_location : Location.t option
    }

  let initial ?last_location inner_state = { inner_state; last_location }

  let[@inline] modify_inner_state f state =
    { state with inner_state = f state.inner_state }

  let enable_backtracking = modify_inner_state State.enable_backtracking

  let disable_backtracking = modify_inner_state State.disable_backtracking

  let[@inline] observe s =
    let open Option in
    State.observe s.inner_state $> fun (t, inner_state') ->
    let location = Token.location t in
    let s' =
      { inner_state = inner_state'; last_location = Option.some location }
    in
    (t, s')

  let can_backtrack ~from ~to_ =
    State.can_backtrack ~from:from.inner_state ~to_:to_.inner_state

  let next_location s =
    let open Option in
    observe s $> fun (token, _s') -> Token.location token

  let previous_location s = s.last_location
end

module Make_parser_with_locations (Token : sig
  type t
end) (State : sig
  include PARSER_STATE with type token = Token.t

  include PARSER_BACKTRACKING_STATE with type t := t

  include
    PARSER_LOCATION_STATE with type t := t and type location = Location.t
end) :
  PARSER_WITH_LOCATIONS
    with type state = State.t
     and type token = Token.t
     and type input = Token.t Seq.t
     and type location = Location.t = struct
  type token = Token.t

  type location = Location.t

  type input = Token.t Seq.t

  type state = State.t

  type +'a t = State.t -> State.t * ('a, exn) result

  type 'a parser = 'a t

  exception Parser_error of exn

  exception
    Labelled_exception of
      { label : string
      ; cause : exn
      }

  exception No_more_choices of exn list

  exception Expected_end_of_input

  let[@inline] run p s = p s

  let[@inline] run_exn p s =
    match run p s with
    | s', Result.Ok e -> (s', e)
    | _s', Result.Error cause -> Error.raise (Parser_error cause)

  let catch p handler s = run p s |> handler

  let fail e s = (s, Result.error e)

  let fail_at_previous_location e s =
    match State.previous_location s with
    | Option.None -> (
        match State.next_location s with
        | Option.None -> fail e s
        | Option.Some next_location ->
            fail
              (Error.located_exception1
                 (Location.start_position_as_location next_location)
                 e)
              s)
    | Option.Some previous_location ->
        fail (Error.located_exception1 previous_location e) s

  let fail_at_next_location e s =
    match State.next_location s with
    | Option.None -> (
        match State.previous_location s with
        | Option.None -> fail e s
        | Option.Some previous_location ->
            fail
              (Error.located_exception1
                 (Location.stop_position_as_location previous_location)
                 e)
              s)
    | Option.Some next_location ->
        fail (Error.located_exception1 next_location e) s

  let return_at s x = (s, Result.ok x)

  let get_state s = return_at s s

  let put_state s _s = return_at s ()

  module M = Monad.Make (struct
    type nonrec 'a t = 'a t

    let return x s = return_at s x

    let bind k p s =
      match run p s with
      | s', Result.Ok x -> run (k x) s'
      | s', (Result.Error _ as e) -> (s', e)
  end)

  include (M : Monad.MONAD with type 'a t := 'a t)

  include (Functor.Make (M) : Functor.FUNCTOR with type 'a t := 'a t)

  include (Apply.Make (M) : Apply.APPLY with type 'a t := 'a t)

  let rec traverse_list f xs =
    match xs with
    | [] -> return []
    | x :: xs ->
        let* y = f x in
        let* ys = traverse_list f xs in
        return (y :: ys)

  let rec traverse_list_void f xs =
    match xs with
    | [] -> return ()
    | x :: xs ->
        let* _ = f x in
        traverse_list_void f xs

  let rec seq_list xs =
    match xs with
    | [] -> return []
    | x :: xs ->
        let* y = x in
        let* ys = seq_list xs in
        return (y :: ys)

  let trying p s =
    match run p s with
    | s, (Result.Error _ as e) ->
        let s' = State.enable_backtracking s in
        (s', e)
    | x -> x

  let label p label =
    catch p (function
      | s, Result.Error (Labelled_exception { cause; _ }) ->
          (s, Result.error (Labelled_exception { cause; label }))
      | s, Result.Error cause ->
          (s, Result.error (Labelled_exception { cause; label }))
      | x -> x)

  let labelled l p = label p l

  let choice ps s =
    let rec go es = function
      | [] -> fail_at_next_location (No_more_choices es)
      | p :: ps' ->
          catch p (function
            | s', Result.Error e when State.can_backtrack ~from:s' ~to_:s ->
                run (go (e :: es) ps') s
            | x -> x)
    in
    run (go [] ps) s

  let[@inline] alt p1 p2 = choice [ p1; p2 ]

  let maybe p = alt (p $> Option.some) (return Option.none)

  let maybe_default p ~default = maybe p $> Option.value ~default

  let void p = p $> fun _x -> ()

  let rec many p = alt (some p $> List1.to_list) (return [])

  and some p =
    let* x = p in
    let* xs = many p in
    return (List1.from x xs)

  let sep_by0 p sep =
    maybe p >>= function
    | Option.None -> return []
    | Option.Some x ->
        let* xs = many (sep &> p) in
        return (x :: xs)

  let sep_by1 p sep =
    let* x = p in
    let* xs = many (sep &> p) in
    return (List1.from x xs)

  let next_location = get_state $> State.next_location

  let previous_location = get_state $> State.previous_location

  let span p =
    let* l1_opt = next_location
    and* x = p
    and* l2_opt = previous_location in
    match (l1_opt, l2_opt) with
    | Option.None, Option.Some l2 -> return (l2, x)
    | Option.Some l1, Option.Some l2 ->
        let l = Location.between ~start:l1 ~stop:l2 in
        return (l, x)
    | _, Option.None ->
        assert
          false (* The parser [p] succeeded, so [l2_opt <> Option.none]. *)

  let eoi s =
    match State.observe s with
    | Option.None -> return_at s ()
    | Option.Some (_token, s') ->
        fail_at_previous_location Expected_end_of_input s'

  let only p = p <& eoi

  let satisfy ~on_token ~on_end_of_input s =
    match State.observe s with
    | Option.None -> on_end_of_input () s
    | Option.Some (token, s') -> (
        match on_token token with
        | Result.Ok r -> return_at s' r
        | Result.Error cause -> fail_at_next_location cause s)
end

module Make (Token : sig
  type t

  val location : t -> Location.t
end) : sig
  include
    PARSER_WITH_LOCATIONS
      with type token = Token.t
       and type input = Token.t Seq.t
       and type location = Location.t

  val initial_state : ?last_location:location -> input -> state
end = struct
  module Simple_state = Make_persistent_bracktracking_state (Token)
  module State_with_locations = Make_location_state (Token) (Simple_state)
  include Make_parser_with_locations (Token) (State_with_locations)

  let initial_state ?last_location input =
    State_with_locations.initial ?last_location (Simple_state.initial input)
end
