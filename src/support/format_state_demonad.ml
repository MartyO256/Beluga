open Format

module type S = sig
  type state

  val pp_flush : state -> Unit.t

  val pp_newline : state -> Unit.t

  val pp_nop : state -> Unit.t

  val pp_cut : state -> Unit.t

  val pp_space : state -> Unit.t

  val pp_non_breaking_space : state -> Unit.t

  val pp_break : state -> Int.t -> Int.t -> Unit.t

  val pp_as : state -> Int.t -> String.t -> Unit.t

  val pp_box : state -> ?indent:Int.t -> (state -> Unit.t) -> Unit.t

  val pp_hbox : state -> (state -> Unit.t) -> Unit.t

  val pp_vbox : state -> ?indent:Int.t -> (state -> Unit.t) -> Unit.t

  val pp_hvbox : state -> ?indent:Int.t -> (state -> Unit.t) -> Unit.t

  val pp_hovbox : state -> ?indent:Int.t -> (state -> Unit.t) -> Unit.t

  val pp_bool : state -> Bool.t -> Unit.t

  val pp_int : state -> Int.t -> Unit.t

  val pp_float : state -> Float.t -> Unit.t

  val pp_char : state -> Char.t -> Unit.t

  val pp_string : state -> String.t -> Unit.t

  val pp_option :
       state
    -> ?none:(state -> Unit.t)
    -> (state -> 'a -> Unit.t)
    -> 'a Option.t
    -> Unit.t

  val pp_list :
       state
    -> ?sep:(state -> Unit.t)
    -> (state -> 'a -> Unit.t)
    -> 'a List.t
    -> Unit.t

  val pp_list1 :
       state
    -> ?sep:(state -> Unit.t)
    -> (state -> 'a -> Unit.t)
    -> 'a List1.t
    -> Unit.t

  val pp_list2 :
       state
    -> ?sep:(state -> Unit.t)
    -> (state -> 'a -> Unit.t)
    -> 'a List2.t
    -> Unit.t

  val pp_text : state -> String.t -> Unit.t

  val pp_utf_8 : state -> String.t -> Unit.t

  val pp_utf_8_text : state -> String.t -> Unit.t

  val traverse_option :
    state -> (state -> 'a -> 'b) -> 'a Option.t -> 'b Option.t

  val traverse_list : state -> (state -> 'a -> 'b) -> 'a List.t -> 'b List.t

  val traverse_list_void :
    state -> (state -> 'a -> Unit.t) -> 'a List.t -> Unit.t

  val traverse_list1 :
    state -> (state -> 'a -> 'b) -> 'a List1.t -> 'b List1.t

  val traverse_list2 :
    state -> (state -> 'a -> 'b) -> 'a List2.t -> 'b List2.t
end

module Make (State : sig
  type state

  val get_formatter : state -> formatter
end) =
struct
  include State

  let nop _state = ()

  let pp_nop state = nop state

  let[@inline] [@specialise] with_formatter state f = f (get_formatter state)

  let pp_flush state =
    with_formatter state (fun ppf -> pp_print_flush ppf ())

  let pp_newline state =
    with_formatter state (fun ppf -> pp_print_newline ppf ())

  let pp_cut state = with_formatter state (fun ppf -> pp_print_cut ppf ())

  let pp_space state =
    with_formatter state (fun ppf -> pp_print_space ppf ())

  let pp_non_breaking_space state =
    with_formatter state (fun ppf -> pp_print_char ppf ' ')

  let pp_break state width offset =
    with_formatter state (fun ppf -> pp_print_break ppf width offset)

  let pp_as state width s =
    with_formatter state (fun ppf -> pp_print_as ppf width s)

  let pp_box state ?(indent = 0) p =
    with_formatter state (fun ppf ->
        pp_open_box ppf indent;
        p state;
        pp_close_box ppf ())

  let pp_hbox state p =
    with_formatter state (fun ppf ->
        pp_open_hbox ppf ();
        p state;
        pp_close_box ppf ())

  let pp_vbox state ?(indent = 0) p =
    with_formatter state (fun ppf ->
        pp_open_vbox ppf indent;
        p state;
        pp_close_box ppf ())

  let pp_hvbox state ?(indent = 0) p =
    with_formatter state (fun ppf ->
        pp_open_hvbox ppf indent;
        p state;
        pp_close_box ppf ())

  let pp_hovbox state ?(indent = 0) p =
    with_formatter state (fun ppf ->
        pp_open_hovbox ppf indent;
        p state;
        pp_close_box ppf ())

  let pp_bool state b = with_formatter state (fun ppf -> pp_print_bool ppf b)

  let pp_int state i = with_formatter state (fun ppf -> pp_print_int ppf i)

  let pp_float state f =
    with_formatter state (fun ppf -> pp_print_float ppf f)

  let pp_char state c = with_formatter state (fun ppf -> pp_print_char ppf c)

  let pp_string state s =
    with_formatter state (fun ppf -> pp_print_string ppf s)

  let pp_option state ?(none = nop) ppv o =
    match o with
    | Option.None -> none state
    | Option.Some x -> ppv state x

  let rec pp_list state ?(sep = nop) ppv l =
    match l with
    | [] -> nop state
    | [ x ] -> ppv state x
    | x :: xs ->
        ppv state x;
        sep state;
        pp_list state ~sep ppv xs

  let pp_list1 state ?(sep = nop) ppv l =
    match l with
    | List1.T (x, []) -> ppv state x
    | List1.T (x, xs) ->
        ppv state x;
        sep state;
        pp_list state ~sep ppv xs

  let pp_list2 state ?(sep = nop) ppv l =
    match l with
    | List2.T (x1, x2, []) ->
        ppv state x1;
        sep state;
        ppv state x2
    | List2.T (x1, x2, xs) ->
        ppv state x1;
        sep state;
        ppv state x2;
        sep state;
        pp_list state ~sep ppv xs

  let pp_text state t = with_formatter state (fun ppf -> pp_print_text ppf t)

  let pp_utf_8 state t = with_formatter state (fun ppf -> pp_utf_8 ppf t)

  let pp_utf_8_text state t =
    with_formatter state (fun ppf -> pp_utf_8_text ppf t)

  let traverse_option state m x_opt =
    match x_opt with
    | Option.None -> Option.none
    | Option.Some x ->
        let y = m state x in
        Option.some y

  let rec traverse_list state f l =
    match l with
    | [] -> []
    | x :: xs ->
        let y = f state x in
        let ys = traverse_list state f xs in
        y :: ys

  let rec traverse_list_void state f l =
    match l with
    | [] -> ()
    | x :: xs ->
        f state x;
        traverse_list_void state f xs

  let traverse_list1 state f (List1.T (x, xs)) =
    let y = f state x in
    let ys = traverse_list state f xs in
    List1.from y ys

  let traverse_list2 state f (List2.T (x1, x2, xs)) =
    let y1 = f state x1 in
    let y2 = f state x2 in
    let ys = traverse_list state f xs in
    List2.from y1 y2 ys
end
