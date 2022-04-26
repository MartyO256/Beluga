(** Linked association hash array mapped tries (HAMT) over ordered types.

    These are finite maps that keep track the sequence of unique bindings
    added to it. This allows for tracing of the additions to the map. *)
module type S = LinkedMap.S

(** Functor building an implementation of the linked HAMT structure given a
    HAMT structure over ordered types. *)
module Make (Hamt : HamtMisc.S) : S with type key = Hamt.key

(** Non-empty linked association hash array mapped tries (HAMT) over ordered
    types.

    These are finite maps that keep track the sequence of unique bindings
    added to it. This allows for tracing of the additions to the map. *)
module type S1 = LinkedMap.S1

(** Functor building an implementation of the non-empty linked HAMT structure
    given a HAMT structure over ordered types. *)
module Make1 (Hamt : HamtMisc.S) : S1 with type key = Hamt.key
