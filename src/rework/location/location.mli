open Support

(** Text file location type and operations. *)

(** The type of locations in a text file. *)
type t

(** Text file position type and operations. *)
module Position : module type of Position

(** {1 Constructors} *)

(** Makes a location over a range of characters.

    @param start_position The start position of the location's range.
    @param end_position The end position of the location's range.
    @param filename The name of location's file. *)
val make :
     filename:string
  -> start_position:Position.t
  -> end_position:Position.t
  -> t

(** Makes a location over a single character.

    @param filename The name of location's file.
    @param position The text file position of the character. *)
val make_from_point : filename:string -> position:Position.t -> t

(** Makes a location over a range of characters.

    @param filename The name of location's file.
    @param range The location's position range. *)
val make_from_range : filename:string -> range:Position.Range.t -> t

(** [initial filename] is the initial location in the file [filename]. *)
val initial : string -> t

(** {1 Destructors} *)

(** [filename location] is the [location]'s file name. *)
val filename : t -> string

(** [range location] is the [location]'s position range. *)
val range : t -> Position.Range.t

(** [start_position location] is the start position of [location]'s range. *)
val start_position : t -> Position.t

(** [end_posiiton location] is the end position of [location]'s range. *)
val end_position : t -> Position.t

(** [start_line location] is the one-based line of [location]'s start
    position. *)
val start_line : t -> int

(** [start_column location] is the one-based column of [location]'s start
    position. *)
val start_column : t -> int

(** [end_line location] is the one-based line of [location]'s end position. *)
val end_line : t -> int

(** [end_column location] is the one-based column of [location]'s end
    position. *)
val end_column : t -> int

(** {1 Instances} *)

include Show.SHOW with type t := t
