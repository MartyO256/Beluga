open Support

module Position = struct
  type t =
    { offset : int
    ; offset_beginning_of_line : int
    ; line : int
    }

  let make ~offset ~offset_beginning_of_line ~line =
    assert (line > 0) ;
    assert (offset >= 0) ;
    assert (offset_beginning_of_line <= offset) ;
    { offset; offset_beginning_of_line; line }


  let[@inline] line { line; _ } = line

  let[@inline] column { offset; offset_beginning_of_line; _ } =
    offset - offset_beginning_of_line + 1


  let initial = make ~line:1 ~offset:0 ~offset_beginning_of_line:0

  let[@inline] line_start_position { offset_beginning_of_line; line; _ } =
    { offset = offset_beginning_of_line; offset_beginning_of_line; line }
end

include Position

include Ord.Make (struct
  include Position

  let[@inline] compare { offset = o1; _ } { offset = o2; _ } =
    Stdlib.compare o1 o2
end)

include Show.Make (struct
  include Position

  let pp ppf position =
    Format.fprintf ppf "line %d, column %d" (line position) (column position)
end)
