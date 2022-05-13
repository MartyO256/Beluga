exception Malformed

exception UnlexableCharacter of string

exception MismatchedBlockComment

exception Violation of string

exception LexingError of Location.t * exn

(** [make filename ?position gen] is the generator for tokens lexed from the
    UTF-8 generator [gen] with locations in the file at [filename] starting
    at position [?position], which defaults to the starting position.

    @raise LexingError If a lexical error is encountered. *)
val make :
     string
  -> ?position:Location.Position.t
  -> char Gen.t
  -> unit
  -> (Location.t * Token.t) option
