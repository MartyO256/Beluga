module type S = sig
  type key

  type 'a t

  val empty : 'a t

  val add : key -> 'a -> 'a t -> 'a t

  val singleton : key -> 'a -> 'a t

  val is_empty : 'a t -> bool

  val mem : key -> 'a t -> bool

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val iter_bindings : (key -> 'a -> unit) -> 'a t -> unit

  val fold_left : ('a -> key -> 'b -> 'a) -> 'b t -> 'a -> 'a

  val fold_right : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val fold_bindings : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val exists : (key -> 'a -> bool) -> 'a t -> bool

  val for_all : (key -> 'a -> bool) -> 'a t -> bool

  val find_opt : key -> 'a t -> 'a option
end

module type S1 = sig
  type key

  type 'a t

  val singleton : key -> 'a -> 'a t

  val add : key -> 'a -> 'a t -> 'a t

  val mem : key -> 'a t -> bool

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val iter_bindings : (key -> 'a -> unit) -> 'a t -> unit

  val fold_left : (key -> 'b -> 'a) -> ('a -> key -> 'b -> 'a) -> 'b t -> 'a

  val fold_right : (key -> 'a -> 'b) -> (key -> 'a -> 'b -> 'b) -> 'a t -> 'b

  val fold_bindings :
    (key -> 'a -> 'b) -> (key -> 'a -> 'b -> 'b) -> 'a t -> 'b

  val exists : (key -> 'a -> bool) -> 'a t -> bool

  val for_all : (key -> 'a -> bool) -> 'a t -> bool

  val find_opt : key -> 'a t -> 'a option
end

module Make (Map : Map.S) : S with type key = Map.key = struct
  type key = Map.key

  type 'a t =
    { list : (key * 'a) List.t
    ; map : 'a Map.t
    }

  let empty = { list = []; map = Map.empty }

  let is_empty { list; _ } = list = []

  let mem key { map; _ } = Map.mem key map

  let find_opt key { map; _ } = Map.find_opt key map

  let add key value ({ list; map } as lmap) =
    find_opt key lmap
    |> Option.eliminate
         (fun () ->
           { list = (key, value) :: list; map = Map.add key value map })
         (Fun.const lmap)

  let singleton key value = add key value empty

  let iter f { list; _ } = List.iter (fun (key, value) -> f key value) list

  let iter_bindings f { map; _ } = Map.iter f map

  let fold_left f { list; _ } init =
    List.fold_left (fun acc (key, value) -> f acc key value) init list

  let fold_right f { list; _ } init =
    List.fold_right (fun (key, value) acc -> f key value acc) list init

  let fold_bindings f { map; _ } init = Map.fold f map init

  let exists p { map; _ } = Map.exists p map

  let for_all p { map; _ } = Map.for_all p map
end

module Make1 (Map : Map.S) : S1 with type key = Map.key = struct
  type key = Map.key

  type 'a t =
    { list : (key * 'a) List1.t
    ; map : 'a Map.t
    }

  let mem key { map; _ } = Map.mem key map

  let find_opt key { map; _ } = Map.find_opt key map

  let add key value ({ list; map } as lmap) =
    find_opt key lmap
    |> Option.eliminate
         (fun () ->
           { list = List1.cons (key, value) list
           ; map = Map.add key value map
           })
         (Fun.const lmap)

  let singleton key value =
    { list = List1.singleton (key, value); map = Map.singleton key value }

  let iter f { list; _ } =
    List1.iter (fun (key, value) -> f key value) list

  let iter_bindings f { map; _ } = Map.iter f map

  let fold_left sing cons =
    List1.fold_left
      (fun (key, value) -> sing key value)
      (fun acc (key, value) -> cons acc key value)
    |> fun go { list; _ } -> go list

  let fold_right sing cons =
    List1.fold_right
      (fun (key, value) -> sing key value)
      (fun (key, value) acc -> cons key value acc)
    |> fun go { list; _ } -> go list

  let fold_bindings sing cons { map; list } =
    let key, value = List1.head list in
    Map.fold cons (Map.remove key map) (sing key value)

  let exists p { map; _ } = Map.exists p map

  let for_all p { map; _ } = Map.for_all p map
end
