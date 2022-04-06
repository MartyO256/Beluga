module type S = LinkedMap.S

module Make (Hamt : HamtMisc.S) : S with type key = Hamt.key = struct
  type key = Hamt.key

  type 'a t =
    { list : (key * 'a) List.t
    ; map : 'a Hamt.t
    }

  let empty = { list = []; map = Hamt.empty }

  let is_empty { list; _ } = list = []

  let mem key { map; _ } = Hamt.mem key map

  let add key value { list; map } =
    { list = (key, value) :: list; map = Hamt.add key value map }

  let singleton key value = add key value empty

  let iter f { list; _ } = List.iter (fun (key, value) -> f key value) list

  let iter_bindings f { map; _ } = Hamt.iter f map

  let fold_left f { list; _ } init =
    List.fold_left (fun acc (key, value) -> f acc key value) init list

  let fold_right f { list; _ } init =
    List.fold_right (fun (key, value) acc -> f key value acc) list init

  let fold_bindings f { map; _ } init = Hamt.fold f map init

  let exists p { map; _ } = Hamt.exists p map

  let for_all p { map; _ } = Hamt.for_all p map

  let find_opt key { map; _ } =
    try Option.some @@ Hamt.find_exn key map with Not_found -> Option.none
end
