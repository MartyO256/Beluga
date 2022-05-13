module L = Location
open Support

exception Malformed = Sedlexing.MalFormed

exception UnlexableCharacter of string

exception MismatchedBlockComment

exception Violation of string

exception LexingError of L.t * exn

let sym_head = [%sedlex.regexp? id_start | '_']

let sym_tail = [%sedlex.regexp? id_continue | Chars "\'-*+@=^/#?"]

let ident = [%sedlex.regexp? sym_head, Star sym_tail]

let digit = [%sedlex.regexp? '0' .. '9']

let number = [%sedlex.regexp? Plus digit]

let hole = [%sedlex.regexp? '?', Opt ident]

let pragma = [%sedlex.regexp? "--", Plus alphabetic]

let hash_ident = [%sedlex.regexp? '#', ident]

let hash_blank = [%sedlex.regexp? "#_"]

let dollar_ident = [%sedlex.regexp? '$', ident]

let dollar_blank = [%sedlex.regexp? "$_"]

let dot_number = [%sedlex.regexp? '.', number]

let arrow = [%sedlex.regexp? "->" | 0x2192]

let turnstile = [%sedlex.regexp? "|-" | 0x22a2]

let thick_arrow = [%sedlex.regexp? "=>" | 0x21d2]

let dots = [%sedlex.regexp? ".." | 0x2026]

let doc_comment_begin = [%sedlex.regexp? "%{{"]

let doc_comment_end = [%sedlex.regexp? "}}%"]

(** Basically, anything that doesn't terminate the block comment. This is
    somewhat tricky to detect. *)
let doc_comment_char =
  [%sedlex.regexp? Compl '}' | '}', Compl '}' | "}}", Compl '%']

let doc_comment =
  [%sedlex.regexp? doc_comment_begin, Star doc_comment_char, doc_comment_end]

let line_comment =
  [%sedlex.regexp?
    '%', Opt (Intersect (Compl '\n', Compl '{'), Star (Compl '\n'))]

let block_comment_begin = [%sedlex.regexp? "%{"]

let block_comment_end = [%sedlex.regexp? "}%"]

let block_comment_char = [%sedlex.regexp? Compl '%' | Compl '}']

let string_delimiter = [%sedlex.regexp? '"']

(* XXX This is stupid and doesn't allow any escape characters. *)
let string_literal =
  [%sedlex.regexp? string_delimiter, Star (Compl '"'), string_delimiter]

(** Skips the _body_ of a block comment. Calls itself recursively upon
    encountering a nested block comment. Consumes the block_comment_end
    symbol. *)
let rec skip_nested_block_comment lexbuf =
  match%sedlex lexbuf with
  | block_comment_begin ->
    (* for the body of the new comment *)
    skip_nested_block_comment lexbuf;
    (* for the remaining characters in this comment *)
    skip_nested_block_comment lexbuf
  | block_comment_end -> ()
  | any ->
    ignore @@ Sedlexing.Utf8.lexeme lexbuf;
    skip_nested_block_comment lexbuf
  | _ ->
    raise
    @@ Violation
         "catch-all case for skip_nested_block_comment should be unreachable"

let rec tokenize lexbuf =
  let const t = Fun.const t (Sedlexing.Utf8.lexeme lexbuf) in
  let module T = Token in
  match%sedlex lexbuf with
  (* comments *)
  | eof -> const T.EOI
  | white_space ->
    ignore @@ Sedlexing.Utf8.lexeme lexbuf;
    tokenize lexbuf
  | block_comment_begin ->
    skip_nested_block_comment lexbuf;
    tokenize lexbuf
  | block_comment_end -> raise MismatchedBlockComment
  | line_comment -> tokenize lexbuf
  (* STRING LITERALS *)
  | string_literal ->
    let s = Sedlexing.Utf8.lexeme lexbuf in
    T.STRING (String.sub s 1 (String.length s - 2))
  (* KEYWORDS *)
  | "and" -> const T.KW_AND
  | "block" -> const T.KW_BLOCK
  | "case" -> const T.KW_CASE
  | "fn" -> const T.KW_FN
  | "else" -> const T.KW_ELSE
  | "if" -> const T.KW_IF
  | "impossible" -> const T.KW_IMPOSSIBLE
  | "in" -> const T.KW_IN
  | "let" -> const T.KW_LET
  | "mlam" -> const T.KW_MLAM
  | "of" -> const T.KW_OF
  | "rec" -> const T.KW_REC
  | "schema" -> const T.KW_SCHEMA
  | "some" -> const T.KW_SOME
  | "then" -> const T.KW_THEN
  | "module" -> const T.KW_MODULE
  | "struct" -> const T.KW_STRUCT
  | "end" -> const T.KW_END
  | "trust" -> const T.KW_TRUST
  | "total" -> const T.KW_TOTAL
  | "type" -> const T.KW_TYPE
  | "ctype" -> const T.KW_CTYPE
  | "prop" -> const T.KW_PROP
  | "inductive" -> const T.KW_INDUCTIVE
  | "coinductive" -> const T.KW_COINDUCTIVE
  | "stratified" -> const T.KW_STRATIFIED
  | "LF" -> const T.KW_LF
  | "fun" -> const T.KW_FUN
  | "typedef" -> const T.KW_TYPEDEF
  | "proof" -> const T.KW_PROOF
  | "by" -> const T.KW_BY
  | "as" -> const T.KW_AS
  | "suffices" -> const T.KW_SUFFICES
  | "toshow" -> const T.KW_TOSHOW
  (* SYMBOLS *)
  | pragma -> T.PRAGMA (String.drop 2 @@ Sedlexing.Utf8.lexeme lexbuf)
  | arrow -> const T.ARROW
  | thick_arrow -> const T.THICK_ARROW
  | turnstile -> const T.TURNSTILE
  | "[" -> const T.LBRACK
  | "]" -> const T.RBRACK
  | "{" -> const T.LBRACE
  | "}" -> const T.RBRACE
  | "(" -> const T.LPAREN
  | ")" -> const T.RPAREN
  | "<" -> const T.LANGLE
  | ">" -> const T.RANGLE
  | "^" -> const T.HAT
  | "," -> const T.COMMA
  | "::" -> const T.DOUBLE_COLON
  | ":" -> const T.COLON
  | ";" -> const T.SEMICOLON
  | "|" -> const T.PIPE
  | "\\" -> const T.LAMBDA
  | "*" -> const T.STAR
  | "=" -> const T.EQUALS
  | "/" -> const T.SLASH
  | "+" -> const T.PLUS
  | hole -> T.HOLE (String.drop 1 (Sedlexing.Utf8.lexeme lexbuf))
  | "_" -> const T.UNDERSCORE
  | ident -> T.IDENT (Sedlexing.Utf8.lexeme lexbuf)
  | dot_number ->
    T.DOT_NUMBER
      (int_of_string (String.drop 1 (Sedlexing.Utf8.lexeme lexbuf)))
  | dots -> const T.DOTS
  | hash_blank -> T.HASH_BLANK
  | hash_ident -> T.HASH_IDENT (Sedlexing.Utf8.lexeme lexbuf)
  | dollar_blank -> T.DOLLAR_BLANK
  | dollar_ident -> T.DOLLAR_IDENT (Sedlexing.Utf8.lexeme lexbuf)
  | "." -> const T.DOT
  | "#" -> const T.HASH
  | "$" -> const T.DOLLAR
  | eof -> const T.EOI
  | number -> T.INTLIT (Sedlexing.Utf8.lexeme lexbuf |> int_of_string)
  | _ -> raise @@ UnlexableCharacter (Sedlexing.Utf8.lexeme lexbuf)

(** [lexbuf_location lexbuf] is the current location of [lexbuf]. *)
let lexbuf_location =
  Fun.(
    Sedlexing.lexing_positions >> fun (s, e) ->
    let filename = s.Lexing.pos_fname in
    let start_point =
      L.Position.make ~offset:s.Lexing.pos_cnum
        ~offset_beginning_of_line:s.Lexing.pos_bol ~line:s.Lexing.pos_lnum
    in
    let end_point =
      L.Position.make ~offset:e.Lexing.pos_cnum
        ~offset_beginning_of_line:e.Lexing.pos_bol ~line:e.Lexing.pos_lnum
    in
    L.make_from_range ~filename
      ~range:(L.Position.Range.make ~start_point ~end_point))

(** [make filename ?position gen] is the generator for tokens lexed from the
    UTF-8 generator [gen] with locations in the file at [filename] starting
    at position [?position], which defaults to the starting position.

    @raise LexingError If a lexical error is encountered. *)
let make filename ?(position = L.Position.initial) gen =
  let lexbuf = Sedlexing.Utf8.from_gen gen in
  (Sedlexing.set_position lexbuf
  @@ Lexing.
       { pos_fname = filename
       ; pos_lnum = L.Position.line position
       ; pos_bol = L.Position.offset_beginning_of_line position
       ; pos_cnum = L.Position.offset position
       });
  let next () =
    try
      let t = tokenize lexbuf in
      Some (lexbuf_location lexbuf, t)
    with
    | ( UnlexableCharacter _
      | MismatchedBlockComment
      | Violation _
      | Malformed ) as e
    ->
      raise @@ LexingError (lexbuf_location lexbuf, e)
  in
  next
