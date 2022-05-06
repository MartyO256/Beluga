open Support

module type RANGE = sig
  type t

  type e

  val make : start_point:e -> end_point:e -> t

  val make_from_point : e -> t

  val join : t -> t -> t

  val start_point : t -> e

  val end_point : t -> e

  val contains : t -> e -> bool

  val includes : t -> t -> bool

  include Eq.EQ with type t := t
end

module Make (Element : Ord.ORD) : RANGE with type e = Element.t = struct
  type e = Element.t

  type t =
    { start_point : e
    ; end_point : e
    }

  let make ~start_point ~end_point =
    assert (Element.(start_point <= end_point));
    { start_point; end_point }

  let[@inline] make_from_point point =
    make ~start_point:point ~end_point:point

  let[@inline] start_point { start_point; _ } = start_point

  let[@inline] end_point { end_point; _ } = end_point

  let join r1 r2 =
    make
      ~start_point:(Element.min (start_point r1) (start_point r2))
      ~end_point:(Element.max (end_point r1) (end_point r2))

  let contains range =
    let start_point = start_point range
    and end_point = end_point range in
    fun element ->
      Element.(start_point <= element) && Element.(element <= end_point)

  let includes outer =
    let contains = contains outer in
    fun inner -> contains (start_point inner) && contains (end_point inner)

  include (
    Eq.Make (struct
      type nonrec t = t

      let equal x y =
        Element.(start_point x = start_point y && end_point x = end_point y)
    end) :
      Eq.EQ with type t := t)
end
