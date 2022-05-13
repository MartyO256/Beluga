open Support
module Position = Position

type t =
  { filename : string
  ; range : Position.Range.t
  }

let make_from_range ~filename ~range = { filename; range }

let make_from_point ~filename ~position =
  make_from_range ~filename ~range:(Position.Range.make_from_point position)

let make ~filename ~start_position ~end_position =
  make_from_range ~filename
    ~range:
      (Position.Range.make ~start_point:start_position
         ~end_point:end_position)

let initial ~filename =
  make_from_range ~filename
    ~range:(Position.Range.make_from_point Position.initial)

let[@inline] filename { filename; _ } = filename

let[@inline] range { range; _ } = range

let start_position = Fun.(range >> Position.Range.start_point)

let end_position = Fun.(range >> Position.Range.end_point)

let start_line = Fun.(start_position >> Position.line)

let end_line = Fun.(end_position >> Position.line)

let start_column = Fun.(start_position >> Position.column)

let end_column = Fun.(end_position >> Position.column)

include (
  Eq.Make (struct
    type nonrec t = t

    let equal x y =
      String.(filename x = filename y) && Position.Range.(range x = range y)
  end) :
    Eq.EQ with type t := t)

include (
  Show.Make (struct
    type nonrec t = t

    let pp ppf location =
      let filename = filename location
      and start_column = start_column location
      and end_column = end_column location
      and start_line = start_line location
      and end_line = end_line location in
      if Stdlib.(start_line = end_line) then
        let line = start_line in
        if Stdlib.(start_column = end_column) then
          Format.fprintf ppf "File \"%s\", line %d, character %d" filename
            line start_column
        else
          Format.fprintf ppf "File \"%s\", line %d, characters %d-%d"
            filename line start_column end_column
      else
        Format.fprintf ppf
          "File \"%s\", from line %d, column %d, to line %d, column %d"
          filename start_line start_column end_line end_column
  end) :
    Show.SHOW with type t := t)
