module type HASH = sig
  type t

  val hash : t -> int
end

module Make (T : sig
  type t
end) : HASH with type t = T.t = struct
  include T

  let hash = Hashtbl.hash
end
