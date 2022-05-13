open Parser

let parse_gen filename g (p : 'a Parser.t) : Parser.state * 'a Parser.result
    =
  let l = Lexer.make filename g |> LinkStream.of_gen in
  Parser.run p (Parser.initial_state l (Location.initial ~filename))

let parse_file filename (p : 'a Parser.t) : Parser.state * 'a Parser.result =
  Gen.IO.with_in filename (fun g -> parse_gen filename g p)

let () =
  let filename = Sys.argv.(1) in
  ignore @@ Parser.extract @@ parse_file filename Parser.sgn
