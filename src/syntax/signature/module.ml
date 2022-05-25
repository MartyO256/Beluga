open Support
open Common

type ('signature, 'entry, 'declaration) t =
  { id : Id.Module.t
  ; name : Name.t
  ; location : Location.t
  ; entries : ('signature * 'entry) List.t
  ; bindings : ('signature * 'declaration) Name.Hamt.t
  ; documentation_comment : DocumentationComment.t Option.t
  }

let make_empty ~id ~location ?documentation_comment name =
  { id
  ; name
  ; location
  ; entries = []
  ; bindings = Name.Hamt.empty
  ; documentation_comment
  }

let add_entry ({ entries; _ } as m) entry =
  { m with entries = List.cons entry entries }

let add_binding ({ bindings; _ } as module_) name binding =
  { module_ with
    bindings =
      bindings |> Name.Hamt.alter name (Fun.const @@ Option.some binding)
  }

let[@inline] id { id; _ } = id

let[@inline] location { location; _ } = location

let[@inline] name { name; _ } = name

let[@inline] entries { entries; _ } = entries

let[@inline] bindings { bindings; _ } = bindings

let[@inline] documentation_comment { documentation_comment; _ } =
  documentation_comment

let lookup m name = m |> bindings |> Name.Hamt.find_opt name

let rec deep_lookup extract current_module module_names base_name =
  match module_names with
  | [] -> lookup current_module base_name
  | head_module_name :: tail_module_names ->
    let open Option in
    lookup current_module head_module_name >>= extract >>= fun m' ->
    deep_lookup extract m' tail_module_names base_name

let fold_entries f init m =
  m |> entries |> List.fold_right (Fun.flip f) |> Fun.apply init
