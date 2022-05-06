open Support

(** Text file position type and operations. *)

(** The type of positions in a text file. *)
type t

(** {1 Constructors} *)

(** Makes a text file position.

    @param offset
      The distance from the position to the beginning of the text file,
      expressed in characters.
    @param offset_beginning_of_line
      The distance from the beginning of the line the position lies in to the
      beginning of the text file, expressed in characters. That is,
      [offset - offset_beginning_of_line + 1] is the one-based column number
      of the position.
    @param line The one-based line number the position lies in.
    @raise Assert_failure if [line <= 0].
    @raise Assert_failure if [offset < 0].
    @raise Assert_failure if [offset < offset_beginning_of_line].*)
val make : offset:int -> offset_beginning_of_line:int -> line:int -> t

(** [initial] is the position at the beginning of a text file. *)
val initial : t

(** {1 Destructors} *)

(** [line p] returns the one-based line number [p] lies in. *)
val line : t -> int

(** [column p] returns the one-based column number [p] lies at. *)
val column : t -> int

(** [line_start_position p] computes the position at the start of the line
    [p] lies in. *)
val line_start_position : t -> t

(** {1 Instances} *)

include Ord.ORD with type t := t

include Show.SHOW with type t := t

(** {1 Collections} *)

module Range : Range.RANGE with type e = t
