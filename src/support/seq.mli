include module type of Stdlib.Seq

(** [cons x xs] is the sequence that begins with the element [x], followed
    with the sequence [xs].

    This may be removed when support is dropped for OCaml < 4.11. *)
val cons : 'a -> 'a t -> 'a t

(** [to_list s] converts [s] to a list. The sequence traversal happens
    immediately and will not terminate on infinite sequences. *)
val to_list : 'a t -> 'a list
