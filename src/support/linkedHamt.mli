module type S = LinkedMap.S

module Make (Hamt : HamtMisc.S) : S with type key = Hamt.key
