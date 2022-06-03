(** Beluga signatures.

    @author Marc-Antoine Ouimet *)

open Support
open Common
module DocumentationComment = DocumentationComment
module Typ = Typ
module Const = Const
module CompTyp = CompTyp
module CompConst = CompConst
module CompCotyp = CompCotyp
module CompDest = CompDest
module Comp = Comp
module Module = Module
module Query = Query
module MQuery = MQuery
module NamePragma = NamePragma

(** Beluga Signatures *)

type mutually_recursive_typs =
  [ `Typs of (Typ.t * Const.t Name.LinkedHamt.t) List1.t ]

type mutually_recursive_comp_typs =
  [ `Comp_typs of
    [ `Comp_typ of CompTyp.t * CompConst.t Name.LinkedHamt.t
    | `Comp_cotyp of CompCotyp.t * CompDest.t Name.LinkedHamt.t
    ]
    List1.t
  ]

type mutually_recursive_programs = [ `Programs of Comp.t Name.LinkedHamt1.t ]

type entry =
  [ `Typ_declaration of Typ.t
  | `Const_declaration of Const.t
  | `Comp_typ_declaration of CompTyp.t
  | `Comp_const_declaration of CompConst.t
  | `Comp_cotyp_declaration of CompCotyp.t
  | `Comp_dest_declaration of CompDest.t
  | `Comp_declaration of Comp.t
  | `Schema_declaration of Schema.t
  | `Module_declaration of (t, entry, declaration) Module.t
  | `Documentation_comment of DocumentationComment.t
  | `Mutually_recursive_declaration of
    [ mutually_recursive_typs
    | mutually_recursive_comp_typs
    | mutually_recursive_programs
    ]
  | `Query_declaration of Query.t
  | `MQuery_declaration of MQuery.t
  | `Name_pragma of NamePragma.t
  ]

and declaration =
  [ `Typ_declaration of Typ.t
  | `Const_declaration of Const.t
  | `Comp_typ_declaration of CompTyp.t
  | `Comp_const_declaration of CompConst.t
  | `Comp_cotyp_declaration of CompCotyp.t
  | `Comp_dest_declaration of CompDest.t
  | `Comp_declaration of Comp.t
  | `Schema_declaration of Schema.t
  | `Module_declaration of (t, entry, declaration) Module.t
  | `Query_declaration of Query.t
  | `MQuery_declaration of MQuery.t
  ]

and t =
  { entries : (t Lazy.t * entry) List.t
  ; bindings : (t Lazy.t * declaration) Name.Hamt.t
  ; typs : (t Lazy.t * Typ.t) Id.Typ.Hamt.t
  ; consts : (t Lazy.t * Const.t) Id.Const.Hamt.t
  ; comp_typs : (t Lazy.t * CompTyp.t) Id.CompTyp.Hamt.t
  ; comp_consts : (t Lazy.t * CompConst.t) Id.CompConst.Hamt.t
  ; comp_cotyps : (t Lazy.t * CompCotyp.t) Id.CompCotyp.Hamt.t
  ; comp_dests : (t Lazy.t * CompDest.t) Id.CompDest.Hamt.t
  ; comps : (t Lazy.t * Comp.t) Id.Comp.Hamt.t
  ; modules : (t Lazy.t * (t, entry, declaration) Module.t) Id.Module.Hamt.t
  ; schemas : (t Lazy.t * Schema.t) Id.Schema.Hamt.t
  ; queries : (t Lazy.t * Query.t) Id.Query.Hamt.t
  ; mqueries : (t Lazy.t * MQuery.t) Id.MQuery.Hamt.t
  ; unfrozen_typs : Id.Typ.Set.t
  ; unfrozen_comp_typs : Id.CompTyp.Set.t
  ; unfrozen_comp_cotyps : Id.CompCotyp.Set.t
  }

type signature = t

(** Destructors *)

let[@inline] entries { entries; _ } = entries

let[@inline] bindings { bindings; _ } = bindings

let[@inline] typs { typs; _ } = typs

let[@inline] consts { consts; _ } = consts

let[@inline] comp_typs { comp_typs; _ } = comp_typs

let[@inline] comp_consts { comp_consts; _ } = comp_consts

let[@inline] comp_cotyps { comp_cotyps; _ } = comp_cotyps

let[@inline] comp_dests { comp_dests; _ } = comp_dests

let[@inline] comps { comps; _ } = comps

let[@inline] modules { modules; _ } = modules

let[@inline] schemas { schemas; _ } = schemas

let[@inline] queries { queries; _ } = queries

let[@inline] mqueries { mqueries; _ } = mqueries

let[@inline] unfrozen_typs { unfrozen_typs; _ } = unfrozen_typs

let[@inline] unfrozen_comp_typs { unfrozen_comp_typs; _ } =
  unfrozen_comp_typs

let[@inline] unfrozen_comp_cotyps { unfrozen_comp_cotyps; _ } =
  unfrozen_comp_cotyps

(** Declaration Guards *)

let guard_typ_declaration : [> `Typ_declaration of Typ.t ] -> Typ.t Option.t
    = function
  | `Typ_declaration declaration -> Option.some declaration
  | _ -> Option.none

let guard_const_declaration :
    [> `Const_declaration of Const.t ] -> Const.t Option.t = function
  | `Const_declaration declaration -> Option.some declaration
  | _ -> Option.none

let guard_comp_typ_declaration :
    [> `Comp_typ_declaration of CompTyp.t ] -> CompTyp.t Option.t = function
  | `Comp_typ_declaration declaration -> Option.some declaration
  | _ -> Option.none

let guard_comp_const_declaration :
    [> `Comp_const_declaration of CompConst.t ] -> CompConst.t Option.t =
  function
  | `Comp_const_declaration declaration -> Option.some declaration
  | _ -> Option.none

let guard_comp_cotyp_declaration :
    [> `Comp_cotyp_declaration of CompCotyp.t ] -> CompCotyp.t Option.t =
  function
  | `Comp_cotyp_declaration declaration -> Option.some declaration
  | _ -> Option.none

let guard_comp_dest_declaration :
    [> `Comp_dest_declaration of CompDest.t ] -> CompDest.t Option.t =
  function
  | `Comp_dest_declaration declaration -> Option.some declaration
  | _ -> Option.none

let guard_comp_declaration :
    [> `Comp_declaration of Comp.t ] -> Comp.t Option.t = function
  | `Comp_declaration declaration -> Option.some declaration
  | _ -> Option.none

let guard_schema_declaration :
    [> `Schema_declaration of Schema.t ] -> Schema.t Option.t = function
  | `Schema_declaration declaration -> Option.some declaration
  | _ -> Option.none

let guard_module_declaration :
       [> `Module_declaration of ('signature, 'entry, 'declaration) Module.t ]
    -> ('signature, 'entry, 'declaration) Module.t Option.t = function
  | `Module_declaration declaration -> Option.some declaration
  | _ -> Option.none

let guard_query_declaration :
    [> `Query_declaration of Query.t ] -> Query.t Option.t = function
  | `Query_declaration query -> Option.some query
  | _ -> Option.none

let guard_mquery_declaration :
    [> `MQuery_declaration of MQuery.t ] -> MQuery.t Option.t = function
  | `MQuery_declaration mquery -> Option.some mquery
  | _ -> Option.none

let extract_declaration guard (signature, declaration_opt) =
  let open Option in
  declaration_opt |> guard $> Pair.left signature

(** Lookups *)

exception UnboundDeclaration of QualifiedName.t * t

exception BoundTypId of Id.Typ.t * t

exception UnboundTypId of Id.Typ.t * t

exception UnboundTyp of QualifiedName.t * t

exception BoundConstId of Id.Const.t * t

exception UnboundConstId of Id.Const.t * t

exception UnboundConst of QualifiedName.t * t

exception BoundCompTypId of Id.CompTyp.t * t

exception UnboundCompTypId of Id.CompTyp.t * t

exception UnboundCompTyp of QualifiedName.t * t

exception BoundCompConstId of Id.CompConst.t * t

exception UnboundCompConstId of Id.CompConst.t * t

exception UnboundCompConst of QualifiedName.t * t

exception BoundCompCotypId of Id.CompCotyp.t * t

exception UnboundCompCotypId of Id.CompCotyp.t * t

exception UnboundCompCotyp of QualifiedName.t * t

exception BoundCompDestId of Id.CompDest.t * t

exception UnboundCompDestId of Id.CompDest.t * t

exception UnboundCompDest of QualifiedName.t * t

exception BoundCompId of Id.Comp.t * t

exception UnboundCompId of Id.Comp.t * t

exception UnboundComp of QualifiedName.t * t

exception BoundSchemaId of Id.Schema.t * t

exception UnboundSchemaId of Id.Schema.t * t

exception UnboundSchema of QualifiedName.t * t

exception BoundModuleId of Id.Module.t * t

exception UnboundModuleId of Id.Module.t * t

exception UnboundModule of QualifiedName.t * t

exception BoundQueryId of Id.Query.t * t

exception UnboundQueryId of Id.Query.t * t

exception UnboundQuery of QualifiedName.t * t

exception BoundMQueryId of Id.MQuery.t * t

exception UnboundMQueryId of Id.MQuery.t * t

exception UnboundMQuery of QualifiedName.t * t

let lookup_typ_by_id_opt' signature id =
  let open Option in
  typs signature |> Id.Typ.Hamt.find_opt id $> Pair.lmap Lazy.force

let lookup_const_by_id_opt' signature id =
  let open Option in
  consts signature |> Id.Const.Hamt.find_opt id $> Pair.lmap Lazy.force

let lookup_comp_typ_by_id_opt' signature id =
  let open Option in
  comp_typs signature |> Id.CompTyp.Hamt.find_opt id $> Pair.lmap Lazy.force

let lookup_comp_const_by_id_opt' signature id =
  let open Option in
  comp_consts signature
  |> Id.CompConst.Hamt.find_opt id
  $> Pair.lmap Lazy.force

let lookup_comp_cotyp_by_id_opt' signature id =
  let open Option in
  comp_cotyps signature
  |> Id.CompCotyp.Hamt.find_opt id
  $> Pair.lmap Lazy.force

let lookup_comp_dest_by_id_opt' signature id =
  let open Option in
  comp_dests signature
  |> Id.CompDest.Hamt.find_opt id
  $> Pair.lmap Lazy.force

let lookup_comp_by_id_opt' signature id =
  let open Option in
  comps signature |> Id.Comp.Hamt.find_opt id $> Pair.lmap Lazy.force

let lookup_schema_by_id_opt' signature id =
  let open Option in
  schemas signature |> Id.Schema.Hamt.find_opt id $> Pair.lmap Lazy.force

let lookup_module_by_id_opt' signature id =
  let open Option in
  modules signature |> Id.Module.Hamt.find_opt id $> Pair.lmap Lazy.force

let lookup_query_by_id_opt' signature id =
  let open Option in
  queries signature |> Id.Query.Hamt.find_opt id $> Pair.lmap Lazy.force

let lookup_mquery_by_id_opt' signature id =
  let open Option in
  mqueries signature |> Id.MQuery.Hamt.find_opt id $> Pair.lmap Lazy.force

let extract_entry_from_lookup_opt' lookup signature id =
  let open Option in
  lookup signature id $> Pair.snd

let lookup_typ_by_id_opt =
  extract_entry_from_lookup_opt' lookup_typ_by_id_opt'

let lookup_const_by_id_opt =
  extract_entry_from_lookup_opt' lookup_const_by_id_opt'

let lookup_comp_typ_by_id_opt =
  extract_entry_from_lookup_opt' lookup_comp_typ_by_id_opt'

let lookup_comp_const_by_id_opt =
  extract_entry_from_lookup_opt' lookup_comp_const_by_id_opt'

let lookup_comp_cotyp_by_id_opt =
  extract_entry_from_lookup_opt' lookup_comp_cotyp_by_id_opt'

let lookup_comp_dest_by_id_opt =
  extract_entry_from_lookup_opt' lookup_comp_dest_by_id_opt'

let lookup_comp_by_id_opt =
  extract_entry_from_lookup_opt' lookup_comp_by_id_opt'

let lookup_schema_by_id_opt =
  extract_entry_from_lookup_opt' lookup_schema_by_id_opt'

let lookup_module_by_id_opt =
  extract_entry_from_lookup_opt' lookup_module_by_id_opt'

let lookup_query_by_id_opt =
  extract_entry_from_lookup_opt' lookup_query_by_id_opt'

let lookup_mquery_by_id_opt =
  extract_entry_from_lookup_opt' lookup_mquery_by_id_opt'

let lookup_typ_by_id' signature id =
  lookup_typ_by_id_opt' signature id
  |> Option.get_or_else (fun () -> raise @@ UnboundTypId (id, signature))

let lookup_const_by_id' signature id =
  lookup_const_by_id_opt' signature id
  |> Option.get_or_else (fun () -> raise @@ UnboundConstId (id, signature))

let lookup_comp_typ_by_id' signature id =
  lookup_comp_typ_by_id_opt' signature id
  |> Option.get_or_else (fun () -> raise @@ UnboundCompTypId (id, signature))

let lookup_comp_const_by_id' signature id =
  lookup_comp_const_by_id_opt' signature id
  |> Option.get_or_else (fun () ->
         raise @@ UnboundCompConstId (id, signature))

let lookup_comp_cotyp_by_id' signature id =
  lookup_comp_cotyp_by_id_opt' signature id
  |> Option.get_or_else (fun () ->
         raise @@ UnboundCompCotypId (id, signature))

let lookup_comp_dest_by_id' signature id =
  lookup_comp_dest_by_id_opt' signature id
  |> Option.get_or_else (fun () ->
         raise @@ UnboundCompDestId (id, signature))

let lookup_comp_by_id' signature id =
  lookup_comp_by_id_opt' signature id
  |> Option.get_or_else (fun () -> raise @@ UnboundCompId (id, signature))

let lookup_schema_by_id' signature id =
  lookup_schema_by_id_opt' signature id
  |> Option.get_or_else (fun () -> raise @@ UnboundSchemaId (id, signature))

let lookup_module_by_id' signature id =
  lookup_module_by_id_opt' signature id
  |> Option.get_or_else (fun () -> raise @@ UnboundModuleId (id, signature))

let lookup_query_by_id' signature id =
  lookup_query_by_id_opt' signature id
  |> Option.get_or_else (fun () -> raise @@ UnboundQueryId (id, signature))

let lookup_mquery_by_id' signature id =
  lookup_mquery_by_id_opt' signature id
  |> Option.get_or_else (fun () -> raise @@ UnboundMQueryId (id, signature))

let extract_entry_from_lookup_by_id' lookup signature id =
  Pair.snd @@ lookup signature id

let lookup_typ_by_id = extract_entry_from_lookup_by_id' lookup_typ_by_id'

let lookup_const_by_id = extract_entry_from_lookup_by_id' lookup_const_by_id'

let lookup_comp_typ_by_id =
  extract_entry_from_lookup_by_id' lookup_comp_typ_by_id'

let lookup_comp_const_by_id =
  extract_entry_from_lookup_by_id' lookup_comp_const_by_id'

let lookup_comp_cotyp_by_id =
  extract_entry_from_lookup_by_id' lookup_comp_cotyp_by_id'

let lookup_comp_dest_by_id =
  extract_entry_from_lookup_by_id' lookup_comp_dest_by_id'

let lookup_comp_by_id = extract_entry_from_lookup_by_id' lookup_comp_by_id'

let lookup_schema_by_id =
  extract_entry_from_lookup_by_id' lookup_schema_by_id'

let lookup_module_by_id =
  extract_entry_from_lookup_by_id' lookup_module_by_id'

let lookup_query_by_id = extract_entry_from_lookup_by_id' lookup_query_by_id'

let lookup_mquery_by_id =
  extract_entry_from_lookup_by_id' lookup_mquery_by_id'

let lookup_name' : t -> Name.t -> (t * declaration) Option.t =
 fun signature name ->
  let open Option in
  signature |> bindings |> Name.Hamt.find_opt name $> Pair.lmap Lazy.force

let lookup_name : t -> Name.t -> declaration Option.t =
 fun signature name ->
  let open Option in
  lookup_name' signature name $> Pair.snd

let lookup_opt' signature qualified_name =
  let base_name = QualifiedName.name qualified_name in
  match QualifiedName.modules qualified_name with
  | [] ->
    (* Lookup top-level declaration in signature *)
    lookup_name' signature base_name
  | head_module_name :: tail_module_names ->
    (* Lookup recursively in modules *)
    let open Option in
    head_module_name |> lookup_name' signature $> Pair.snd
    >>= guard_module_declaration
    >>= fun top_module ->
    Module.deep_lookup
      Fun.(Pair.snd >> guard_module_declaration)
      top_module tail_module_names base_name

let lookup_opt signature qualified_name =
  let open Option in
  lookup_opt' signature qualified_name $> Pair.snd

let guarded_declaration_lookup guard signature =
  let open Option in
  lookup_opt' signature >=> extract_declaration guard

let lookup_typ_opt' = guarded_declaration_lookup guard_typ_declaration

let lookup_const_opt' = guarded_declaration_lookup guard_const_declaration

let lookup_comp_typ_opt' =
  guarded_declaration_lookup guard_comp_typ_declaration

let lookup_comp_const_opt' =
  guarded_declaration_lookup guard_comp_const_declaration

let lookup_comp_cotyp_opt' =
  guarded_declaration_lookup guard_comp_cotyp_declaration

let lookup_comp_dest_opt' =
  guarded_declaration_lookup guard_comp_dest_declaration

let lookup_comp_opt' = guarded_declaration_lookup guard_comp_declaration

let lookup_schema_opt' = guarded_declaration_lookup guard_schema_declaration

let lookup_module_opt' = guarded_declaration_lookup guard_module_declaration

let lookup_query_opt' = guarded_declaration_lookup guard_query_declaration

let lookup_mquery_opt' = guarded_declaration_lookup guard_mquery_declaration

let extract_entry_from_lookup_opt' lookup signature id =
  let open Option in
  lookup signature id $> Pair.snd

let lookup_typ_opt = extract_entry_from_lookup_opt' lookup_typ_opt'

let lookup_const_opt = extract_entry_from_lookup_opt' lookup_const_opt'

let lookup_comp_typ_opt = extract_entry_from_lookup_opt' lookup_comp_typ_opt'

let lookup_comp_const_opt =
  extract_entry_from_lookup_opt' lookup_comp_const_opt'

let lookup_comp_cotyp_opt =
  extract_entry_from_lookup_opt' lookup_comp_cotyp_opt'

let lookup_comp_dest_opt =
  extract_entry_from_lookup_opt' lookup_comp_dest_opt'

let lookup_comp_opt = extract_entry_from_lookup_opt' lookup_comp_opt'

let lookup_schema_opt = extract_entry_from_lookup_opt' lookup_schema_opt'

let lookup_module_opt = extract_entry_from_lookup_opt' lookup_module_opt'

let lookup_query_opt = extract_entry_from_lookup_opt' lookup_query_opt'

let lookup_mquery_opt = extract_entry_from_lookup_opt' lookup_mquery_opt'

let lookup' signature qualified_name =
  lookup_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundDeclaration (qualified_name, signature))

let lookup_typ' signature qualified_name =
  lookup_typ_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundTyp (qualified_name, signature))

let lookup_const' signature qualified_name =
  lookup_const_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundConst (qualified_name, signature))

let lookup_comp_typ' signature qualified_name =
  lookup_comp_typ_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundCompTyp (qualified_name, signature))

let lookup_comp_const' signature qualified_name =
  lookup_comp_const_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundCompConst (qualified_name, signature))

let lookup_comp_cotyp' signature qualified_name =
  lookup_comp_cotyp_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundCompCotyp (qualified_name, signature))

let lookup_comp_dest' signature qualified_name =
  lookup_comp_dest_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundCompDest (qualified_name, signature))

let lookup_comp' signature qualified_name =
  lookup_comp_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundComp (qualified_name, signature))

let lookup_schema' signature qualified_name =
  lookup_schema_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundSchema (qualified_name, signature))

let lookup_module' signature qualified_name =
  lookup_module_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundModule (qualified_name, signature))

let lookup_query' signature qualified_name =
  lookup_query_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundQuery (qualified_name, signature))

let lookup_mquery' signature qualified_name =
  lookup_mquery_opt' signature qualified_name
  |> Option.get_or_else (fun () ->
         raise @@ UnboundMQuery (qualified_name, signature))

let extract_entry_from_lookup' lookup signature id =
  Pair.snd @@ lookup signature id

let lookup = extract_entry_from_lookup' lookup'

let lookup_typ = extract_entry_from_lookup' lookup_typ'

let lookup_const = extract_entry_from_lookup' lookup_const'

let lookup_comp_typ = extract_entry_from_lookup' lookup_comp_typ'

let lookup_comp_const = extract_entry_from_lookup' lookup_comp_const'

let lookup_comp_cotyp = extract_entry_from_lookup' lookup_comp_cotyp'

let lookup_comp_dest = extract_entry_from_lookup' lookup_comp_dest'

let lookup_comp = extract_entry_from_lookup' lookup_comp'

let lookup_schema = extract_entry_from_lookup' lookup_schema'

let lookup_module = extract_entry_from_lookup' lookup_module'

let lookup_query = extract_entry_from_lookup' lookup_query'

let lookup_mquery = extract_entry_from_lookup' lookup_mquery'

module Subordination = struct
  open Internal

  (** The type of subordination state looked up from the signature. *)
  type old_subordinations =
    { lookup_kind : Id.Typ.t -> LF.kind
          (** [lookup_kind tA_id] is the kind corresponding to the LF family
              having ID [tA_id]. *)
    ; lookup_constructors : Id.Typ.t -> LF.typ list
          (** [lookup_constructors tA_id] are the constructors corresponding
              to the LF family having ID [tA_id]. *)
    ; is_term_subordinate_known : Id.Typ.t -> Id.Typ.t -> bool Option.t
          (** [is_term_subordinate_known tA tB] is

              - [Some true] if [tB] is a known term-level subordinate to
                [tA],
              - [Some false] if [tB] is known not to be a term-level
                subordinate to [tA],
              - [None] if the term-level subordination of [tA] and [tB] is
                unknown.

              This function is constructed from an existing Beluga signature
              when the initial subordination state is created with
              {!initial_state}. *)
    ; is_type_subordinate_to_known : Id.Typ.t -> Id.Typ.t -> bool Option.t
          (** [is_type_subordinate_to_known tA tB] is

              - [Some true] if [tB] is a known type-level subordinate to
                [tA],
              - [Some false] if [tB] is known not to be a type-level
                subordinate to [tA],
              - [None] if the type-level subordination of [tA] and [tB] is
                unknown.

              This function is constructed from an existing Beluga signature
              when the initial subordination state is created with
              {!initial_state}. *)
    }

  (** The type of newly discovered subordination state. *)
  type new_subordinations =
    { new_term_subordinations : Id.Typ.Set.t Id.Typ.Hamt.t
          (** The mapping from LF families to their term-level subordinates.*)
    ; new_type_subordinations : Id.Typ.Set.t Id.Typ.Hamt.t
          (** The mapping from LF families to their type-level subordinates. *)
    }

  type state = old_subordinations * new_subordinations

  include (
    State.Make (struct
      type t = state
    end) :
      State.STATE with type state := state)

  let[@inline] lookup_kind ({ lookup_kind; _ }, _) = lookup_kind

  let[@inline] lookup_constructors ({ lookup_constructors; _ }, _) =
    lookup_constructors

  let[@inline] is_term_subordinate_known ({ is_term_subordinate_known; _ }, _)
      =
    is_term_subordinate_known

  let[@inline] is_type_subordinate_to_known
      ({ is_type_subordinate_to_known; _ }, _) =
    is_type_subordinate_to_known

  let[@inline] new_term_subordinations (_, { new_term_subordinations; _ }) =
    new_term_subordinations

  let[@inline] new_type_subordinations (_, { new_type_subordinations; _ }) =
    new_type_subordinations

  let initial_state :
         lookup_kind:(Id.Typ.t -> LF.kind)
      -> lookup_constructors:(Id.Typ.t -> LF.typ list)
      -> is_term_subordinate_known:(Id.Typ.t -> Id.Typ.t -> bool Option.t)
      -> is_type_subordinate_to_known:(Id.Typ.t -> Id.Typ.t -> bool Option.t)
      -> state =
   fun ~lookup_kind ~lookup_constructors ~is_term_subordinate_known
       ~is_type_subordinate_to_known ->
    ( { lookup_kind
      ; lookup_constructors
      ; is_term_subordinate_known
      ; is_type_subordinate_to_known
      }
    , { new_term_subordinations = Id.Typ.Hamt.empty
      ; new_type_subordinations = Id.Typ.Hamt.empty
      } )

  let lookup_kind tA = get $> Fun.(lookup_kind >> Fun.apply tA)

  let lookup_constructors tA =
    get $> Fun.(lookup_constructors >> Fun.apply tA)

  let lookup_old_term_subordinations tA tB =
    get $> Fun.(is_term_subordinate_known >> Fun.apply tA >> Fun.apply tB)

  let lookup_old_type_subordinations tA tB =
    get $> Fun.(is_type_subordinate_to_known >> Fun.apply tA >> Fun.apply tB)

  let lookup_new_term_subordinations tA =
    get $> Fun.(new_term_subordinations >> Id.Typ.Hamt.find_opt tA)

  let lookup_new_type_subordinations tA =
    get $> Fun.(new_type_subordinations >> Id.Typ.Hamt.find_opt tA)

  (** [lookup_is_term_subordinate tA tB state] is

      - [Some true] if [tB] is a term-level subordinate to [tA] with respect
        to [state],
      - [Some false] if [tB] is not a term-level subordinate to [tA] with
        respect to [state],
      - [None] if the term-level subordination of [tA] and [tB] is unknown
        with respect to [state].

      The term-level subordination of [tA] and [tB] is determined by first
      looking up in the new subordination relations and then the old ones. *)
  let lookup_is_term_subordinate tA tB =
    lookup_new_term_subordinations tA
    >>= Option.eliminate
          (fun () -> lookup_old_term_subordinations tA tB)
          Fun.(Id.Typ.Set.mem tB >> Option.some >> return)

  (** [lookup_is_type_subordinate tA tB state] is

      - [Some true] if [tB] is a type-level subordinate to [tA] with respect
        to [state],
      - [Some false] if [tB] is not a type-level subordinate to [tA] with
        respect to [state],
      - [None] if the type-level subordination of [tA] and [tB] is unknown
        with respect to [state].

      The type-level subordination of [tA] and [tB] is determined by first
      looking up in the new subordination relations and then the old ones. *)
  let lookup_is_type_subordinate_to tA tB =
    lookup_new_type_subordinations tA
    >>= Option.eliminate
          (fun () -> lookup_old_type_subordinations tA tB)
          Fun.(Id.Typ.Set.mem tB >> Option.some >> return)

  let compute_subordinations : Id.Typ.t -> state -> new_subordinations =
    let compute_subordinations : Id.Typ.t -> new_subordinations t =
     fun _ _ -> raise @@ Invalid_argument "[compute_subordinations tA state]"
    in
    fun tA state -> run ~init:state (compute_subordinations tA) |> Pair.snd
end

let empty_subordination_state : t -> Subordination.state =
 fun signature ->
  let lookup_kind = Fun.(lookup_typ_by_id signature >> Typ.kind) in
  let lookup_constructors =
    Fun.(
      lookup_typ_by_id signature
      >> Typ.constructors >> Name.Hamt.values
      >> List.map (lookup_const_by_id signature >> Const.typ))
  in
  let is_term_subordinate_known tA_id tB_id =
    let tA = lookup_typ_by_id signature tA_id in
    try Option.some @@ Typ.is_term_subordinate tA tB_id
    with Typ.UnfrozenTyp _ -> Option.none
  in
  let is_type_subordinate_to_known tA_id tB_id =
    let tA = lookup_typ_by_id signature tA_id in
    try Option.some @@ Typ.is_type_subordinate_to tA tB_id
    with Typ.UnfrozenTyp _ -> Option.none
  in
  Subordination.initial_state ~lookup_kind ~lookup_constructors
    ~is_term_subordinate_known ~is_type_subordinate_to_known

module Mutation = struct
  (** The type of mutations to a signature.

      Mutations may lazily refer to the signature resulting from applying the
      current mutation. Mutations must not force their second argument since
      in {!val:Mutation.apply}, that argument is analogous to a null
      reference. *)
  type t = signature -> signature Lazy.t -> signature

  type mutation = t

  (** [identity] performs no mutation on the input signature. *)
  let identity : mutation = fun signature _ -> signature

  (** [sequence_list mutations] constructs the mutation that performs the
      mutations in [mutations] in order. *)
  let sequence_list : mutation List.t -> mutation =
   fun mutations signature signature' ->
    List.fold_left
      (fun signature mutation -> mutation signature signature')
      signature mutations

  (** [sequence_seq mutations] constructs the mutation that sequentially
      performs the mutations in [mutations]. *)
  let sequence_seq : mutation Seq.t -> mutation =
   fun mutations signature signature' ->
    Seq.fold_left
      (fun signature mutation -> mutation signature signature')
      signature mutations

  (** [apply signature mutation] calls [mutation] on [signature] and on the
      mutation result recursively. *)
  let apply : signature -> mutation -> signature =
   fun signature mutation ->
    let rec signature' = lazy (mutation signature signature') in
    Lazy.force signature'

  (** [apply_list signature mutations] sequences the mutations [mutations]
      and applies them on [signature]. *)
  let apply_list : signature -> mutation List.t -> signature =
   fun signature mutations -> sequence_list mutations |> apply signature

  let add_entry : entry -> mutation =
   fun entry signature signature' ->
    { signature with
      entries = entries signature |> List.cons (signature', entry)
    }

  let add_binding : Name.t -> declaration -> mutation =
   fun name declaration signature signature' ->
    { signature with
      bindings =
        bindings signature
        |> Name.Hamt.alter name
             (Fun.const @@ Option.some (signature', declaration))
    }

  let add_binding_opt : Name.t Option.t -> declaration -> mutation =
   fun name_opt declaration signature signature' ->
    match name_opt with
    | None -> identity signature signature'
    | Some name -> add_binding name declaration signature signature'

  let add_typ : Typ.t -> mutation =
   fun tA signature signature' ->
    { signature with
      typs = Id.Typ.Hamt.add (Typ.id tA) (signature', tA) (typs signature)
    }

  let add_const : Const.t -> mutation =
   fun tM signature signature' ->
    { signature with
      consts =
        Id.Const.Hamt.add (Const.id tM) (signature', tM) (consts signature)
    }

  let add_comp_typ : CompTyp.t -> mutation =
   fun cA signature signature' ->
    { signature with
      comp_typs =
        Id.CompTyp.Hamt.add (CompTyp.id cA) (signature', cA)
          (comp_typs signature)
    }

  let add_comp_const : CompConst.t -> mutation =
   fun cM signature signature' ->
    { signature with
      comp_consts =
        Id.CompConst.Hamt.add (CompConst.id cM) (signature', cM)
          (comp_consts signature)
    }

  let add_comp_cotyp : CompCotyp.t -> mutation =
   fun cA signature signature' ->
    { signature with
      comp_cotyps =
        Id.CompCotyp.Hamt.add (CompCotyp.id cA) (signature', cA)
          (comp_cotyps signature)
    }

  let add_comp_dest : CompDest.t -> mutation =
   fun cM signature signature' ->
    { signature with
      comp_dests =
        Id.CompDest.Hamt.add (CompDest.id cM) (signature', cM)
          (comp_dests signature)
    }

  let add_comp : Comp.t -> mutation =
   fun p signature signature' ->
    { signature with
      comps = Id.Comp.Hamt.add (Comp.id p) (signature', p) (comps signature)
    }

  let add_module : (signature, entry, declaration) Module.t -> mutation =
   fun m signature signature' ->
    { signature with
      modules =
        Id.Module.Hamt.add (Module.id m) (signature', m) (modules signature)
    }

  let add_query : Query.t -> mutation =
   fun query signature signature' ->
    { signature with
      queries =
        Id.Query.Hamt.add (Query.id query) (signature', query)
          (queries signature)
    }

  let add_mquery : MQuery.t -> mutation =
   fun mquery signature signature' ->
    { signature with
      mqueries =
        Id.MQuery.Hamt.add (MQuery.id mquery) (signature', mquery)
          (mqueries signature)
    }

  let add_schema : Schema.t -> mutation =
   fun schema signature signature' ->
    { signature with
      schemas =
        Id.Schema.Hamt.add (Schema.id schema) (signature', schema)
          (schemas signature)
    }

  let add_unfrozen_typ_if : bool -> Id.Typ.t -> mutation =
   fun cond id signature signature' ->
    if cond then
      { signature with
        unfrozen_typs = Id.Typ.Set.add id (unfrozen_typs signature)
      }
    else identity signature signature'

  let add_unfrozen_comp_typ_if : bool -> Id.CompTyp.t -> mutation =
   fun cond id signature signature' ->
    if cond then
      { signature with
        unfrozen_comp_typs =
          Id.CompTyp.Set.add id (unfrozen_comp_typs signature)
      }
    else identity signature signature'

  let add_unfrozen_comp_cotyp_if : bool -> Id.CompCotyp.t -> mutation =
   fun cond id signature signature' ->
    if cond then
      { signature with
        unfrozen_comp_cotyps =
          Id.CompCotyp.Set.add id (unfrozen_comp_cotyps signature)
      }
    else identity signature signature'

  let update_typ : Typ.t -> mutation =
   fun tA signature signature' ->
    { signature with
      typs =
        Id.Typ.Hamt.alter (Typ.id tA)
          (Fun.const @@ Option.some (signature', tA))
          (typs signature)
    }

  let update_typs : Typ.t Id.Typ.Hamt.t -> mutation =
   fun tAs signature signature' ->
    { signature with
      typs =
        Id.Typ.Hamt.merge
          (fun _ tA' tA ->
            Option.alt Option.(tA' $> Pair.left signature') tA)
          tAs (typs signature)
    }

  let update_comp_typ : CompTyp.t -> mutation =
   fun cA signature signature' ->
    { signature with
      comp_typs =
        Id.CompTyp.Hamt.alter (CompTyp.id cA)
          (Fun.const @@ Option.some (signature', cA))
          (comp_typs signature)
    }

  let update_comp_typs : CompTyp.t Id.CompTyp.Hamt.t -> mutation =
   fun cAs signature signature' ->
    { signature with
      comp_typs =
        Id.CompTyp.Hamt.merge
          (fun _ cA' cA ->
            Option.alt Option.(cA' $> Pair.left signature') cA)
          cAs (comp_typs signature)
    }

  let update_comp_cotyp : CompCotyp.t -> mutation =
   fun cA signature signature' ->
    { signature with
      comp_cotyps =
        Id.CompCotyp.Hamt.alter (CompCotyp.id cA)
          (Fun.const @@ Option.some (signature', cA))
          (comp_cotyps signature)
    }

  let update_comp_cotyps : CompCotyp.t Id.CompCotyp.Hamt.t -> mutation =
   fun cAs signature signature' ->
    { signature with
      comp_cotyps =
        Id.CompCotyp.Hamt.merge
          (fun _ cA' cA ->
            Option.alt Option.(cA' $> Pair.left signature') cA)
          cAs (comp_cotyps signature)
    }

  let freeze_typs : Id.Typ.Set.t -> mutation =
   fun tAs signature _ ->
    { signature with
      unfrozen_typs = Id.Typ.Set.diff (unfrozen_typs signature) tAs
    }

  let freeze_comp_typs : Id.CompTyp.Set.t -> mutation =
   fun cAs signature _ ->
    { signature with
      unfrozen_comp_typs =
        Id.CompTyp.Set.diff (unfrozen_comp_typs signature) cAs
    }

  let freeze_comp_cotyps : Id.CompCotyp.Set.t -> mutation =
   fun cAs signature _ ->
    { signature with
      unfrozen_comp_cotyps =
        Id.CompCotyp.Set.diff (unfrozen_comp_cotyps signature) cAs
    }

  (** [freeze_typ tA] is the mutation that freezes at least the LF family
      declaration having ID [tA] only if it is unfrozen. If that declaration
      is already frozen, then the input signature is returned as is.

      The term-level and type-level subordination relations for [tA] are
      computed in the process, which may cause other LF family declarations
      to be frozen as well. *)
  let freeze_typ : Typ.t -> mutation =
   fun tA signature signature' ->
    if Typ.is_frozen tA then signature
    else
      let { Subordination.new_term_subordinations; new_type_subordinations }
          =
        Subordination.compute_subordinations (Typ.id tA)
          (empty_subordination_state signature)
      in
      let replacements =
        Id.Typ.Hamt.merge
          (fun tA_id term_subordinations type_subordinations ->
            Option.some
            @@ Typ.freeze
                 ~term_subordinates:
                   (Option.value ~default:Id.Typ.Set.empty
                      term_subordinations)
                 ~type_subordinated_to:
                   (Option.value ~default:Id.Typ.Set.empty
                      type_subordinations)
                 (lookup_typ_by_id signature tA_id))
          new_term_subordinations new_type_subordinations
      in
      let newly_frozen_declarations =
        replacements |> Id.Typ.Hamt.keys |> Id.Typ.Set.of_list
      in
      sequence_list
        [ update_typs replacements; freeze_typs newly_frozen_declarations ]
        signature signature'

  let freeze_typ_by_id : Id.Typ.t -> mutation =
   fun id signature signature' ->
    freeze_typ (lookup_typ_by_id signature id) signature signature'

  let freeze_typ_by_ids : Id.Typ.t List.t -> mutation =
   fun ids signature signature' ->
    List.fold_left
      (fun signature id -> freeze_typ_by_id id signature signature')
      signature ids

  (** [freeze_comp_typ cA] is the mutation that freezes at least the
      computational-level data type constant declaration having ID [cA] only
      if it is unfrozen. If that declaration is already frozen, then the
      input signature is returned as is. *)
  let freeze_comp_typ : CompTyp.t -> mutation =
   fun cA signature signature' ->
    let cA_id = CompTyp.id cA in
    try
      let cA' = CompTyp.freeze cA in
      sequence_list
        [ update_comp_typs (Id.CompTyp.Hamt.singleton cA_id cA')
        ; freeze_comp_typs (Id.CompTyp.Set.singleton cA_id)
        ]
        signature signature'
    with CompTyp.FrozenCompTyp _ -> identity signature signature'

  let freeze_comp_typ_by_id : Id.CompTyp.t -> mutation =
   fun id signature signature' ->
    freeze_comp_typ (lookup_comp_typ_by_id signature id) signature signature'

  let freeze_comp_typ_by_ids : Id.CompTyp.t List.t -> mutation =
   fun ids signature signature' ->
    List.fold_left
      (fun signature id -> freeze_comp_typ_by_id id signature signature')
      signature ids

  (** [freeze_comp_cotyp cA] is the mutation that freezes at least the
      computational-level codata type constant declaration having ID [cA]
      only if it is unfrozen. If that declaration is already frozen, then the
      input signature is returned as is. *)
  let freeze_comp_cotyp : CompCotyp.t -> mutation =
   fun cA signature signature' ->
    let cA_id = CompCotyp.id cA in
    try
      let cA' = CompCotyp.freeze cA in
      sequence_list
        [ update_comp_cotyps (Id.CompCotyp.Hamt.singleton cA_id cA')
        ; freeze_comp_cotyps (Id.CompCotyp.Set.singleton cA_id)
        ]
        signature signature'
    with CompCotyp.FrozenCompCotyp _ -> identity signature signature'

  let freeze_comp_cotyp_by_id : Id.CompCotyp.t -> mutation =
   fun id signature signature' ->
    freeze_comp_cotyp
      (lookup_comp_cotyp_by_id signature id)
      signature signature'

  let freeze_comp_cotyp_by_ids : Id.CompCotyp.t List.t -> mutation =
   fun ids signature signature' ->
    List.fold_left
      (fun signature id -> freeze_comp_cotyp_by_id id signature signature')
      signature ids

  let freeze_declaration : declaration -> mutation = function
    | `Typ_declaration declaration -> freeze_typ declaration
    | `Comp_typ_declaration declaration -> freeze_comp_typ declaration
    | `Comp_cotyp_declaration declaration -> freeze_comp_cotyp declaration
    | _ -> identity

  (** [freeze_declaration_by_name name] is the mutation that freezes at least
      the declaration having name [name] only if it is unfrozen. If that
      declaration is already frozen, then the input signature is returned as
      is. *)
  let freeze_declaration_by_name : Name.t -> mutation =
   fun name signature signature' ->
    let open Option in
    lookup_name signature name
    $> (fun declaration ->
         freeze_declaration declaration signature signature')
    |> Option.value ~default:signature

  (** [freeze_declaration_by_name_opt name_opt] is the mutation that freezes
      at least the declaration having name [name] only if it is unfrozen and
      [name_opt] is [Some name]. If that declaration is already frozen or
      [name_opt] is [None], then the input signature is returned as is. *)
  let freeze_declaration_by_name_opt : Name.t Option.t -> mutation =
    Option.eliminate (Fun.const identity) freeze_declaration_by_name

  (** [freeze_all_unfrozen_declarations] is the mutation that freezes all
      unfrozen declarations in the signature. *)
  let freeze_all_unfrozen_declarations : mutation =
   fun signature ->
    sequence_list
      [ freeze_typ_by_ids (typs signature |> Id.Typ.Hamt.keys)
      ; freeze_comp_typ_by_ids (comp_typs signature |> Id.CompTyp.Hamt.keys)
      ; freeze_comp_cotyp_by_ids
          (comp_cotyps signature |> Id.CompCotyp.Hamt.keys)
      ]
      signature
end

let add_typ signature tA =
  let tA_id = Typ.id tA in
  try
    ignore @@ lookup_typ_by_id signature tA_id;
    raise @@ BoundTypId (tA_id, signature)
  with UnboundTypId _ ->
    let tA_name = Typ.name tA
    and tA_declaration = `Typ_declaration tA in
    Mutation.(
      apply_list signature
        [ freeze_declaration_by_name tA_name
        ; add_entry tA_declaration
        ; add_binding tA_name tA_declaration
        ; add_typ tA
        ; add_unfrozen_typ_if (Typ.is_unfrozen tA) tA_id
        ])

let add_const signature tM =
  let tM_id = Const.id tM in
  try
    ignore @@ lookup_const_by_id signature tM_id;
    raise @@ BoundConstId (tM_id, signature)
  with UnboundConstId _ ->
    let tK = lookup_typ_by_id signature (Const.kind tM)
    and tM_name = Const.name tM in
    let tK' = Typ.add_constructor tM_name tM_id tK in
    let tM_declaration = `Const_declaration tM in
    Mutation.(
      apply_list signature
        [ update_typ tK'
        ; freeze_declaration_by_name tM_name
        ; add_entry tM_declaration
        ; add_binding tM_name tM_declaration
        ; add_const tM
        ])

let add_comp_typ signature cA =
  let cA_id = CompTyp.id cA in
  try
    ignore @@ lookup_comp_typ_by_id signature cA_id;
    raise @@ BoundCompTypId (cA_id, signature)
  with UnboundCompTypId _ ->
    let cA_name = CompTyp.name cA
    and cA_declaration = `Comp_typ_declaration cA in
    Mutation.(
      apply_list signature
        [ freeze_declaration_by_name cA_name
        ; add_entry cA_declaration
        ; add_binding cA_name cA_declaration
        ; add_comp_typ cA
        ; add_unfrozen_comp_typ_if (CompTyp.is_unfrozen cA) cA_id
        ])

let add_comp_const signature cM =
  let cM_id = CompConst.id cM in
  try
    ignore @@ lookup_comp_const_by_id signature cM_id;
    raise @@ BoundCompConstId (cM_id, signature)
  with UnboundCompConstId _ ->
    let cK = lookup_comp_typ_by_id signature (CompConst.kind cM)
    and cM_name = CompConst.name cM in
    let cK' = CompTyp.add_constructor cM_name cM_id cK in
    let cM_declaration = `Comp_const_declaration cM in
    Mutation.(
      apply_list signature
        [ update_comp_typ cK'
        ; freeze_declaration_by_name cM_name
        ; add_entry cM_declaration
        ; add_binding cM_name cM_declaration
        ; add_comp_const cM
        ])

let add_comp_cotyp signature cA =
  let cA_id = CompCotyp.id cA in
  try
    ignore @@ lookup_comp_cotyp_by_id signature cA_id;
    raise @@ BoundCompCotypId (cA_id, signature)
  with UnboundCompCotypId _ ->
    let cA_name = CompCotyp.name cA
    and cA_declaration = `Comp_cotyp_declaration cA in
    Mutation.(
      apply_list signature
        [ freeze_declaration_by_name cA_name
        ; add_entry cA_declaration
        ; add_binding cA_name cA_declaration
        ; add_comp_cotyp cA
        ; add_unfrozen_comp_cotyp_if (CompCotyp.is_unfrozen cA) cA_id
        ])

let add_comp_dest signature cM =
  let cM_id = CompDest.id cM in
  try
    ignore @@ lookup_comp_dest_by_id signature cM_id;
    raise @@ BoundCompDestId (cM_id, signature)
  with UnboundCompDestId _ ->
    let cK = lookup_comp_cotyp_by_id signature (CompDest.kind cM)
    and cM_name = CompDest.name cM in
    let cK' = CompCotyp.add_destructor cM_name cM_id cK in
    let cM_declaration = `Comp_dest_declaration cM in
    Mutation.(
      apply_list signature
        [ update_comp_cotyp cK'
        ; freeze_declaration_by_name cM_name
        ; add_entry cM_declaration
        ; add_binding cM_name cM_declaration
        ; add_comp_dest cM
        ])

let add_query signature query =
  let query_id = Query.id query in
  try
    ignore @@ lookup_query_by_id signature query_id;
    raise @@ BoundQueryId (query_id, signature)
  with UnboundQueryId _ ->
    let query_name = Query.name query
    and query_declaration = `Query_declaration query in
    Mutation.(
      apply_list signature
        [ freeze_all_unfrozen_declarations
        ; add_entry query_declaration
        ; add_binding_opt query_name query_declaration
        ; add_query query
        ])

let add_mquery signature mquery =
  let mquery_id = MQuery.id mquery in
  try
    ignore @@ lookup_mquery_by_id signature mquery_id;
    raise @@ BoundMQueryId (mquery_id, signature)
  with UnboundMQueryId _ ->
    let mquery_name = MQuery.name mquery
    and mquery_declaration = `MQuery_declaration mquery in
    Mutation.(
      apply_list signature
        [ freeze_all_unfrozen_declarations
        ; add_entry mquery_declaration
        ; add_binding_opt mquery_name mquery_declaration
        ; add_mquery mquery
        ])

let add_name_pragma signature pragma =
  let tA_id = NamePragma.typ pragma in
  let tA = lookup_typ_by_id signature tA_id in
  let tA' =
    Typ.set_naming_conventions
      ~var:(NamePragma.var_naming_convention pragma)
      ~mvar:(Option.some @@ NamePragma.mvar_naming_convention pragma)
      tA
  in
  Mutation.(apply signature @@ update_typ tA')

let add_documentation_comment signature comment =
  Mutation.(apply signature @@ add_entry (`Documentation_comment comment))

let empty =
  { entries = []
  ; bindings = Name.Hamt.empty
  ; typs = Id.Typ.Hamt.empty
  ; consts = Id.Const.Hamt.empty
  ; comp_typs = Id.CompTyp.Hamt.empty
  ; comp_consts = Id.CompConst.Hamt.empty
  ; comp_cotyps = Id.CompCotyp.Hamt.empty
  ; comp_dests = Id.CompDest.Hamt.empty
  ; comps = Id.Comp.Hamt.empty
  ; modules = Id.Module.Hamt.empty
  ; schemas = Id.Schema.Hamt.empty
  ; queries = Id.Query.Hamt.empty
  ; mqueries = Id.MQuery.Hamt.empty
  ; unfrozen_typs = Id.Typ.Set.empty
  ; unfrozen_comp_typs = Id.CompTyp.Set.empty
  ; unfrozen_comp_cotyps = Id.CompCotyp.Set.empty
  }

let find_all_queries signature =
  Id.Query.Hamt.values (queries signature) |> List.map (Pair.lmap Lazy.force)

let find_all_mqueries signature =
  Id.MQuery.Hamt.values (mqueries signature)
  |> List.map (Pair.lmap Lazy.force)
