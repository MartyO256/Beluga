open Support
open Id
open Syntax.Int

module OpPragmas : sig
  type fixPragma =
    { name : Name.t
    ; fix : Fixity.t
    ; precedence : int
    ; assoc : Associativity.t option
    }

  val default : Associativity.t ref

  val clear : unit -> unit

  val addPragma : Name.t -> Fixity.t -> int option -> Associativity.t option-> unit

  val getPragma : Name.t -> fixPragma option

  val pragmaExists : Name.t -> bool

  val pragmaCount : int ref
end

module Modules : sig
  type state = Id.module_id * string list * Id.module_id list * (string * string list) list

  val abbrevs : (string * string list) list ref
  val addAbbrev : string list -> string -> unit

  val getState : unit -> state
  val setState : state -> unit

  val current : module_id ref
  val currentName : string list ref
  val opened : module_id list ref

  val addSgnToCurrent : Sgn.decl -> unit

  val directory : (string list, module_id) Hashtbl.t
  val modules : Sgn.decl list ref DynArray.t

  val id_of_name : string list -> module_id
  val name_of_id : module_id -> string list

  val instantiateModule : string -> module_id
  val open_module : string list -> module_id

  val reset : unit -> unit
end

module type ENTRY = sig
  type t
  val name_of_entry : t -> Name.t

  type cid = Id.module_id * int
end

module Cid : sig
  module Typ : sig
    module Entry : sig
      type t =
        private
          { name : Name.t
          ; implicit_arguments : int
          ; kind : LF.kind
          ; var_generator : (unit -> string) option
          ; mvar_generator : (unit -> string) option
          ; frozen : bool ref
          ; constructors : Id.cid_term list ref
          ; subordinates : BitSet.t ref
          ; typesubordinated : BitSet.t ref
          ; decl : Decl.t
          }
    end
    type entry = Entry.t

    val freeze : cid_typ -> unit

    (* val entry_list : (Id.cid_typ list ref) DynArray.t *)

    val mk_entry : Name.t -> LF.kind -> int -> entry
    val add : (cid_typ -> entry) -> cid_typ
    val set_name_convention : cid_typ
                              -> (unit -> string) option
                              -> (unit -> string) option
                              -> unit
    val gen_var_name : LF.typ -> (unit -> string) option
    val gen_mvar_name : LF.typ -> (unit -> string) option
    val get : cid_typ -> entry
    val index_of_name : Name.t -> cid_typ
    val addConstructor : Syntax.Loc.t -> cid_typ -> cid_term -> LF.typ -> unit
    val clear : unit -> unit
    val args_of_name : Name.t -> int
    val args_of_name_opt : Name.t -> int option
    val current_entries : unit -> (cid_typ * entry) list

    (* see subord.ml for an explanation of term-level subordination
       and type-level subordination *)
    val is_subordinate_to : cid_typ -> cid_typ -> bool       (* term-level *)
    val is_typesubordinate_to : cid_typ -> cid_typ -> bool   (* type-level (dependent arguments) *)
  end


  module Term : sig
    module Entry : sig
      type t =
        private
          { name : Name.t
          ; implicit_arguments : int
          ; typ : LF.typ
          ; decl : Decl.t
          }
      type cid = Id.cid_term
      val name_of_entry : t -> Name.t
    end
    type entry = Entry.t

    val mk_entry : Name.t -> LF.typ -> int -> entry
    val get : cid_term -> entry
    val add' : Location.t -> cid_typ -> (cid_term -> entry) -> cid_term
    val get_implicit_arguments : cid_term -> int
    val index_of_name : Name.t -> cid_term
    val args_of_name : Name.t -> int
    val args_of_name_opt : Name.t -> int option
    val clear : unit -> unit
  end

  module CompTyp : sig
    module Entry : sig
      type t =
        private
          { name : Name.t
          ; implicit_arguments : int
          ; kind : Comp.kind
          ; positivity : Sgn.positivity_flag
          ; mutable frozen : bool
          ; constructors : cid_comp_const list ref
          ; decl : Decl.t
          }
      val name_of_entry : t -> Name.t
      type cid = Id.cid_comp_typ
    end
    type entry = Entry.t

    (* val entry_list : (Id.cid_comp_typ list ref) DynArray.t *)
    val mk_entry : Name.t -> Comp.kind -> int -> Sgn.positivity_flag -> entry
    val current_entries : unit -> (cid_comp_typ * entry) list
    val add : (cid_comp_typ -> entry) -> cid_comp_typ
    val get : cid_comp_typ -> entry
    val freeze : cid_comp_typ -> unit
    val addConstructor : cid_comp_const -> cid_comp_typ -> unit
    val index_of_name : Name.t -> cid_comp_typ
    val index_of_name_opt : Name.t -> cid_comp_typ option
    val clear : unit -> unit
    val get_implicit_arguments : cid_comp_typ -> int
  end

  module CompCotyp : sig
    module Entry : sig
      type t =
        private
          { name : Name.t
          ; implicit_arguments : int
          ; kind : Comp.kind
          ; frozen : bool ref
          ; destructors : cid_comp_dest list ref
          ; decl : Decl.t
          }
      type cid = Id.cid_comp_cotyp
      val name_of_entry : t -> Name.t
    end
    type entry = Entry.t

    val mk_entry : Name.t -> Comp.kind -> int -> entry

    val add : (cid_comp_cotyp -> entry) -> cid_comp_cotyp
    val fixed_name_of : cid_comp_cotyp -> Name.t
    val get : cid_comp_cotyp -> entry
    val current_entries : unit -> (cid_comp_cotyp * entry) list
    val freeze : cid_comp_cotyp -> unit
    val addDestructor : cid_comp_dest -> cid_comp_cotyp -> unit
    val index_of_name : Name.t -> cid_comp_typ
    val index_of_name_opt : Name.t -> cid_comp_typ option
    val clear : unit -> unit
  end

  module CompConst : sig
    module Entry : sig
      type t =
        private
          { name : Name.t
          ; implicit_arguments : int
          ; typ : Comp.typ
          ; decl : Decl.t
          }
      type cid = Id.cid_comp_const
      val name_of_entry : t -> Name.t
    end
    type entry = Entry.t

    val mk_entry : Name.t -> Comp.typ -> int -> entry
    val add : cid_comp_typ -> (cid_comp_const -> entry) -> cid_comp_const
    val current_entries : unit -> (cid_comp_const * entry) list
    val get : cid_comp_const -> entry
    val get_implicit_arguments : cid_comp_const -> int
    val index_of_name : Name.t -> cid_comp_const
    val index_of_name_opt : Name.t -> cid_comp_const option
    val clear : unit -> unit
  end

  module CompDest : sig
    module Entry : sig
      type t =
        private
          { name : Name.t
          ; implicit_arguments : int
          ; mctx : LF.mctx
          ; obs_type : Comp.typ
          ; return_type : Comp.typ
          ; decl : Decl.t
          }
      type cid = Id.cid_comp_dest
      val name_of_entry : t -> Name.t
    end
    type entry = Entry.t

    val mk_entry : Name.t -> LF.mctx -> Comp.typ -> Comp.typ -> int -> entry
    val add : cid_comp_cotyp -> (cid_comp_dest -> entry) -> cid_comp_dest
    val current_entries : unit -> (cid_comp_dest * entry) list
    val get : cid_comp_dest -> entry
    val fixed_name_of : cid_comp_dest -> Name.t
    val get_implicit_arguments : cid_comp_dest -> int
    val index_of_name : Name.t -> cid_comp_dest
    val index_of_name_opt : Name.t -> cid_comp_dest option
    val clear : unit -> unit
  end

  module CompTypDef : sig
    module Entry : sig
      type t =
        private
          { name : Name.t
          ; implicit_arguments : int
          ; kind : Comp.kind
          ; mctx : LF.mctx
          ; typ : Comp.typ
          ; decl : Decl.t
          }
      type cid = Id.cid_comp_typdef
      val name_of_entry : t -> Name.t
    end
    type entry = Entry.t

    val mk_entry : Name.t -> int -> (LF.mctx * Comp.typ) -> Comp.kind -> entry
    val add : (cid_comp_typdef -> entry) -> cid_comp_typdef
    val current_entries : unit -> (cid_comp_typdef * entry) list
    val get : cid_comp_typdef -> entry
    val fixed_name_of : cid_comp_typdef -> Name.t
    val get_implicit_arguments : cid_comp_typdef -> int
    val index_of_name : Name.t -> cid_comp_typdef
    val index_of_name_opt : Name.t -> cid_comp_typdef option
    val clear : unit -> unit
  end

  module Comp : sig
    val add_mutual_group : Comp.total_dec list -> cid_mutual_group

    module Entry : sig
      type t =
        { name : Name.t
        ; implicit_arguments : int
        ; typ : Comp.typ
        ; prog : Comp.value option
        ; mutual_group : cid_mutual_group
        ; decl : Decl.t option
        }
      type cid = Id.cid_prog
      val name_of_entry : t -> Name.t
    end
    type entry = Entry.t

    (** Gets the Name.t of the given theorem ID. *)
    val name : cid_prog -> Name.t

    (** Looks up the total declarations for the given mutual group. *)
    val lookup_mutual_group : cid_mutual_group -> Comp.total_dec list

    (** Gets the mutual group ID for a given function reference. *)
    val mutual_group : cid_prog -> cid_mutual_group

    (** Looks up the total declarations for the mutual group of the
        given function.
     *)
    val total_decs : cid_prog -> Comp.total_dec list

    (** Looks up the totality declaration for the given theorem. *)
    val get_total_decl : cid_prog -> Comp.total_dec

    val mk_entry : Decl.t option ->
                   Name.t -> Comp.typ -> int
                   -> cid_mutual_group -> Comp.value option
                   -> entry

    (** If the value we store in the entry is a recursive value, it
        itself needs the cid_prog that we are creating to store this
        entry. Therefore, unlike 'add' functions in other modules,
        this 'add' function expects a function to which it will
        provide the cid_prog it generated to store the entry, thus
        tying the recursive knot.
     *)
    val add : (cid_prog -> entry) -> cid_prog
    val get : cid_prog -> entry
    val current_entries : unit -> (cid_prog * entry) list

    val fixed_name_of : cid_prog -> Name.t
    val index_of_name : Name.t -> cid_prog
    val index_of_name_opt : Name.t -> cid_prog option

    (* val entry_list : ((Id.cid_prog * Loc.t) list ref) DynArray.t *)
    val clear : unit -> unit

    (** Update the associated program of an existing entry.
        This is notably used for two-step elaboration of functions:
        - a function is added to the store with its type *before* its
          bodies is elaborated
        - indexing can find the cid of the function immediately from
          the store.
        - when the function body is elaborated, the store entry is
          updated to hold the body as well.
     *)
    val set_prog : Id.cid_prog -> (Comp.value option -> Comp.value option) -> unit

    (** Modifies the declaration number of a theorem. *)
    val set_decl : Id.cid_prog -> (Decl.t option -> Decl.t option) -> unit
  end

  module Schema : sig
    module Entry : sig
      type t =
        private
          { name : Name.t
          ; schema : LF.schema
          ; decl : Decl.t
          }
      val name_of_entry : t -> Name.t
      type cid = Id.cid_schema
    end
    type entry = Entry.t

    val mk_entry : Name.t -> LF.schema -> entry
    val add : (cid_schema -> entry) -> cid_schema
    val get : cid_schema -> entry
    val get_schema : cid_schema -> LF.schema
    val index_of_name : Name.t -> cid_schema
    val get_name : cid_schema -> Name.t
    val clear : unit -> unit
  end

  module type RENDERER = sig
    open Id
    open Syntax.Int
    val render_cid_comp_typ : cid_comp_typ -> string
    val render_cid_comp_cotyp : cid_comp_cotyp -> string
    val render_cid_comp_const : cid_comp_const -> string
    val render_cid_comp_dest : cid_comp_dest -> string
    val render_cid_typ : cid_typ -> string
    val render_cid_term : cid_term -> string
    val render_cid_schema : cid_schema -> string
    val render_cid_prog : cid_prog -> string
    val render_cid_mutual_group : cid_mutual_group -> string

    val render_offset : offset -> string
    val render_ctx_var : LF.mctx -> offset -> string
    val render_cvar : LF.mctx -> offset -> string
    val render_bvar : LF.dctx -> offset -> string
    val render_var : Comp.gctx -> var -> string
  end

  (* Default RENDERER for Internal Syntax *)
  module DefaultRenderer : RENDERER

  (* Named Renderer for Internal Syntax *)
  module NamedRenderer : RENDERER
end

val clear : unit -> unit

(** Persistent stack-based store for LF bound variables.

    Used for indexing. *)
module BVar : sig
  type entry =
    private
      { name : Name.t
      }

  val mk_entry : Name.t -> entry
  type t (* NOTE: t is an ordered data structure *)
  val empty : t
  val extend : t -> entry -> t
  val get : t -> var -> entry
  val length : t -> int
  val index_of_name : t -> Name.t -> offset
end


module FVar : sig
  val add : Name.t -> LF.typ -> unit
  val get : Name.t -> LF.typ
  val clear : unit -> unit
end

module FCVar : sig
  val add : Name.t -> LF.mctx * LF.ctyp_decl -> unit
  val get : Name.t -> LF.mctx * LF.ctyp_decl
  val clear : unit -> unit
end

(** Persistent stack-based store for computational variables.

    Used for indexing. *)
module Var : sig
  type entry =
    { name : Name.t
    }

  val mk_entry : Name.t -> entry
  type t (* NOTE: t is an ordered data structure *)
  val to_list : t -> entry list
  val empty : t
  val extend : t -> entry -> t
  val get : t -> var -> entry
  val append : t -> t -> t
  val index_of_name : t -> Name.t -> offset
  val size : t -> int

  (** Erases the context down to a list of names. *)
  val of_gctx : Comp.gctx -> t
  val of_list : Name.t list -> t
end

(** Persistent stack-based store for contextual variables.

    Used for indexing. *)
module CVar : sig
  type cvar = Name.t

  type entry =
    { name : cvar
    ; plicity : Plicity.t
    }

  val mk_entry : cvar -> Plicity.t -> entry

  type t (* NOTE: t is an ordered data structure *)

  val empty : t
  val extend : t -> entry -> t
  val get : t -> var -> entry

  (** [index_of_name scope name] is [(plicity, offset)] where [offset] is the
      de Bruijn index of [name] in [scope], and [plicity] is whether that
      variable was explicitly declared or not.
      @raises Not_found if no such variable is in scope. *)
  val index_of_name : t -> cvar -> Plicity.t * offset
  val append : t -> t -> t
  val length : t -> int

  (** Erases the context down to a list of names using the given
      function to interpret the plicity of declarations. *)
  val of_mctx : (Plicity.t * Inductivity.t -> Plicity.t) -> LF.mctx -> t

  val to_string : t -> string
  val of_list : (Name.t * Plicity.t) list -> t
end
