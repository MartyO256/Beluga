open Support

(** Text file position type and operations. *)

(** The type of positions in a text file. *)
type t

(** {1 Constructors} *)

(** [make ~offset ~offset_beginning_of_line ~line] makes a text file
    position, where [~offset] is the distance from the position to the
    beginning of the text file, expressed in characters,
    [~offset_beginning_of_line] is the distance from the beginning of the
    line the position lies in to the beginning of the text file, expressed in
    characters, and [~line] is the one-based line number the position lies
    in.

    [offset - offset_beginning_of_line + 1] is the one-based column number of
    the position.

    @raise Assert_failure If [line <= 0].
    @raise Assert_failure If [offset < 0].
    @raise Assert_failure If [offset < offset_beginning_of_line].*)
val make : offset:int -> offset_beginning_of_line:int -> line:int -> t

(** [initial] is the position at the beginning of a text file. *)
val initial : t

(** {1 Destructors} *)

(** [offset position] is the number of characters from the beginning of the
    text file and [position]. *)
val offset : t -> int

(** [offset_beginning_of_line position] is the number of characters from the
    beginning of the text file and the start of the line containing
    [position]. *)
val offset_beginning_of_line : t -> int

(** [line p] returns the one-based line number [p] lies in. *)
val line : t -> int

(** [column p] returns the one-based column number [p] lies at. *)
val column : t -> int

(** [line_start_position p] computes the position at the start of the line
    [p] lies in. *)
val line_start_position : t -> t

(** {1 Instances} *)

include Eq.EQ with type t := t

include Ord.ORD with type t := t

include Show.SHOW with type t := t

(** {1 Collections} *)

module Range : Range.RANGE with type e = t
