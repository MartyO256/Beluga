module GenericOrder = struct
  type 'a t =
    | Arg of 'a
    | Lex of 'a t list
    | Simul of 'a t list

  let rec map f = function
    | Arg x ->
        Arg (f x)
    | Lex xs ->
        Lex (List.map (map f) xs)
    | Simul xs ->
        Simul (List.map (map f) xs)
end

include GenericOrder
