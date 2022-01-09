open Support
module Position = Position
module PositionRange = Range.Make (Position)

module Location = struct
  type t =
    { filename : string
    ; range : PositionRange.t
    }

  let make_from_range ~filename ~range = { filename; range }

  let make_from_point ~filename ~position =
    make_from_range ~filename ~range:(PositionRange.make_from_point position)


  let make ~filename ~start_position ~end_position =
    make_from_range
      ~filename
      ~range:
        (PositionRange.make
           ~start_point:start_position
           ~end_point:end_position )


  let initial filename =
    make_from_range
      ~filename
      ~range:(PositionRange.make_from_point Position.initial)


  let[@inline] filename { filename; _ } = filename

  let[@inline] range { range; _ } = range

  let[@inline] start_position location =
    location |> range |> PositionRange.start_point


  let[@inline] end_position location =
    location |> range |> PositionRange.end_point

  let[@inline] start_line location =
    location |> start_position |> Position.line

  let[@inline] end_line location = location |> end_position |> Position.line

  let[@inline] start_column location =
    location |> start_position |> Position.column

  let[@inline] end_column location =
    location |> end_position |> Position.column
end

include Location

include Show.Make (struct
  include Location

  let pp ppf location =
    let filename = filename location
    and start_column = start_column location
    and end_column = end_column location
    and start_line = start_line location
    and end_line = end_line location in
    if start_line = end_line
    then
      let line = start_line in
      if start_column = end_column
      then
        Format.fprintf
          ppf
          "File \"%s\", line %d, character %d"
          filename
          line
          start_column
      else
        Format.fprintf
          ppf
          "File \"%s\", line %d, characters %d-%d"
          filename
          line
          start_column
          end_column
    else
      Format.fprintf
        ppf
        "File \"%s\", from line %d, column %d, to line %d, column %d"
        filename
        start_line
        start_column
        end_line
        end_column
end)
