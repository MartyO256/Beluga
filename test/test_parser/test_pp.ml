open Support
open Beluga_syntax
open Util
open Base_json
open Synext_json
open Assert
open Beluga_parser.Mutable

let parse_only_signature_file =
  parse_and_disambiguate
    ~parser:Parsing.(only signature_file)
    ~disambiguator:Disambiguation.disambiguate_signature_file

exception Empty_configuration_file

let pp_json_signature_file ppf signature_file =
  Yojson.Safe.pretty_print ~std:true ppf
    (Base_json.without_locations
       (Synext_json.json_of_signature_file signature_file))

let save_signature_file_json ~filename signature_file =
  Support.Files.with_pp_to_file filename (fun ppf ->
      Format.fprintf ppf "%a@." pp_json_signature_file signature_file)

let replace_filename_extension filename ~extension =
  Filename.remove_extension filename ^ extension

let replace_location_filename_extension location ~extension =
  replace_filename_extension (Location.filename location) ~extension

let filename_json location =
  replace_location_filename_extension location ~extension:".json"

let filename_pp location =
  replace_location_filename_extension location ~extension:".pp"

let save_signature_files_json =
  List1.iter (fun signature_file ->
      save_signature_file_json
        ~filename:(filename_json signature_file.Synext.Signature.location)
        signature_file)

let save_signature_file_pp =
  let open Synext.Printer in
  fun x state ->
    Support.Files.with_pp_to_file (filename_pp x.Synext.Signature.location)
      (fun ppf ->
        run (set_formatter ppf &> pp_signature_file x ++ pp_newline) state)

let save_signature_files_pp =
  let open Synext.Printer in
  fun (List1.T (x, xs)) ->
    let state, () =
      Support.Files.with_pp_to_file (filename_pp x.Synext.Signature.location)
        (fun ppf -> run (save_signature_file_pp x) (make_initial_state ppf))
    in
    eval (traverse_list_void save_signature_file_pp xs) state

let pp_and_parse_signature_files =
  let module Disambiguation_state =
    Beluga_parser.Mutable.Disambiguation_state
  in
  let module Printer = Synext.Printer in
  let module Parser = Beluga_parser.Mutable in
  fun (List1.T (x, xs)) ->
    let buffer = Buffer.create 16 in
    let printing_state =
      Printer.make_initial_state (Format.formatter_of_buffer buffer)
    in
    let printing_state', () =
      Printer.(run (pp_signature_file x ++ pp_newline) printing_state)
    in
    let parser_state, y =
      run parse_only_signature_file
        (make_initial_state_from_string
           ~disambiguation_state:
             (Disambiguation_state.create_initial_state ())
           ~initial_location:Location.ghost ~input:(Buffer.contents buffer))
    in
    let _printing_state'', _parser_state', ys_rev =
      List.fold_left
        (fun (printing_state, parser_state, ys_rev) x ->
          Buffer.clear buffer;
          let printing_state', () =
            Printer.(run (pp_signature_file x ++ pp_newline) printing_state)
          in
          let parser_state', () =
            set_parser_state
              (make_initial_parser_state_from_string
                 ~initial_location:Location.ghost (Buffer.contents buffer))
              parser_state
          in
          let parser_state'', y =
            run parse_only_signature_file parser_state'
          in
          (printing_state', parser_state'', y :: ys_rev))
        (printing_state', parser_state, [])
        xs
    in
    List1.from y (List.rev ys_rev)

let assert_equal_as_json f ~expected ~actual =
  assert_json_equal ~expected:(f expected) ~actual:(f actual)

let make_compiler_test ?(save_json_to_file = true) ?(save_pp_to_file = false)
    compiler_test_file =
  let open OUnit2 in
  compiler_test_file >:: fun _test_ctxt ->
  let beluga_source_files =
    Beluga_parser.Config_parser.read_configuration
      ~filename:compiler_test_file
  in
  match beluga_source_files with
  | [] -> Error.raise Empty_configuration_file
  | x :: xs ->
      let signature_source_files = List1.map Pair.snd (List1.from x xs) in
      let signature =
        Beluga_parser.Simple.read_multi_file_signature signature_source_files
      in
      if save_json_to_file then save_signature_files_json signature;
      if save_pp_to_file then save_signature_files_pp signature;
      let signature' = pp_and_parse_signature_files signature in
      assert_equal_as_json
        Fun.(json_of_signature >> without_locations)
        ~expected:signature ~actual:signature'

let examples_directory = "../examples"

let tests () =
  let compiler_tests =
    Files.find_compiler_tests ~directory:examples_directory
  in
  List.map make_compiler_test compiler_tests
