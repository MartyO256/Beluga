exception Unsupported_sort of string

exception Unsupported_fixity of string

exception Unsupported_associativity of string

val read_disambiguation_state :
  string -> Beluga_parser.Simple_disambiguation_state.t
