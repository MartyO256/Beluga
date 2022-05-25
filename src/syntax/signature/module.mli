(** Namespace for declarations. *)

open Support
open Common

type ('signature, 'entry, 'declaration) t

(** {1 Constructors} *)

val make_empty :
     id:Id.Module.t
  -> location:Location.t
  -> ?documentation_comment:DocumentationComment.t
  -> Name.t
  -> (_, _, _) t

val add_entry :
     ('signature, 'entry, 'declaration) t
  -> 'signature * 'entry
  -> ('signature, 'entry, 'declaration) t

val add_binding :
     ('signature, 'entry, 'declaration) t
  -> Name.t
  -> 'signature * 'declaration
  -> ('signature, 'entry, 'declaration) t

(** {1 Destructors} *)

val id : (_, _, _) t -> Id.Module.t

val location : (_, _, _) t -> Location.t

val name : (_, _, _) t -> Name.t

val entries : ('signature, 'entry, _) t -> ('signature * 'entry) List.t

val documentation_comment : (_, _, _) t -> DocumentationComment.t Option.t

(** {1 Lookups} *)

val lookup :
     ('signature, _, 'declaration) t
  -> Name.t
  -> ('signature * 'declaration) Option.t

val deep_lookup :
     (   'signature * 'declaration
      -> ('signature, 'entry, 'declaration) t Option.t)
  -> ('signature, 'entry, 'declaration) t
  -> Name.t List.t
  -> Name.t
  -> ('signature * 'declaration) Option.t

(** {1 Iterators} *)

val fold_entries :
  ('a -> 'signature * 'entry -> 'a) -> 'a -> ('signature, 'entry, _) t -> 'a
