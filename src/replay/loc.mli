(** A location inside an input stream.
*)
type t

(** [line location] is the [location]'s line. *)
val line : t -> int

(** [offset location] is the [location]'s character offset from the beginning
    of the file. *)
val offset : t -> int

(** [beginning_of_line location] is the [location]'s character offset from the
    beginning of the file to the beginning of the line in which the located
    character appears. *)
val beginning_of_line : t -> int

(** [shift offset location] is [location] shifted by [offset] characters. *)
val shift : int -> t -> t

(** Computes what column the location refers to within its line. *)
val column : t -> int

(** Produces a string representation of the location. *)
val to_string : t -> string

(** Compares two locations.
    If the first comes before the second, then the result is negative.
    If the locations are equal, then the result is zero.
    If the first comes after the second, then the result is positive.
 *)
val compare : t -> t -> int

(** Increases the location by analyzing the given character.
    If the character is a line break, then the line number is
    incremented and the beginning-of-line offset is set to the
    current offset.
 *)
val inc_by_char : char -> t -> t

(** The initial location. Used to kick-start parsers.
    Specifically, this location has
    {! line = 1 !}, {! offset = 1 !}, and {! bol = 1 !}.
 *)
val initial : t
