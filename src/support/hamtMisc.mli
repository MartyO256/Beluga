include module type of Hamt

module type HASH_TYPE = sig
  type t

  include Ord.ORD with type t := t

  include Hash.HASH with type t := t
end

module Make (Key : HASH_TYPE) : S with type key = Key.t
