open Support
open OUnit2

(** [readdir_list d] reads the directory [d] to a list of file names. *)
let readdir_list = Fun.(Sys.readdir >> Array.to_list)

(** [readdir_list_concat d] reads the directory [d] to a list of file names
    with [d] concatenated to them. *)
let readdir_list_concat directory =
  readdir_list directory |> List.map (Filename.concat directory)

(** [read_all_subdirectories d] is the list of all file names to
    subdirectories of [d] relative to [d]. *)
let read_all_subdirectories =
  let rec read_all_subdirectories directories_to_read subdirectories =
    match directories_to_read with
    | directory :: directories ->
      let new_subdirectories =
        readdir_list_concat directory |> List.filter Sys.is_directory
      in
      read_all_subdirectories
        (new_subdirectories @ directories)
        (new_subdirectories @ subdirectories)
    | [] -> subdirectories
  in
  fun directories_to_read -> read_all_subdirectories directories_to_read []

let is_file_with_suffix suffix name = Filename.check_suffix name suffix

let is_cfg_file = is_file_with_suffix ".cfg"

let is_bel_file = is_file_with_suffix ".bel"

(** [read_test_files d] recursively reads the list of test files in [d]. If a
    subdirectory of [d] contains a [*.cfg] file, then those files are used as
    tests for that subdirectory. Otherwise, all [*.bel] files are used. *)
let read_test_files =
  let read_test_files_in_directory directory =
    let files = readdir_list_concat directory in
    let cfgs = List.filter is_cfg_file files in
    if List.null cfgs then List.filter is_bel_file files else cfgs
  in
  fun directory ->
    read_all_subdirectories [ directory ]
    |> List.concat_map read_test_files_in_directory

let compiler_success_test_case_from_file admissible_fail file =
  file >:: fun _ ->
  skip_if (String.Set.mem file admissible_fail) "admissible fail";
  ignore @@ Beluga.Load.load Fmt.null_formatter file

let read_compiler_success_tests admissible_fail directory =
  read_test_files directory
  |> List.map (compiler_success_test_case_from_file admissible_fail)

let compiler_success_tests label admissible_fail examples_directory =
  label >::: read_compiler_success_tests admissible_fail examples_directory

exception UnexpectedSuccess

let compiler_error_test_case_from_file admissible_fail file =
  file >:: fun _ ->
  skip_if (String.Set.mem file admissible_fail) "admissible fail";
  try
    ignore @@ Beluga.Load.load Fmt.null_formatter file;
    raise UnexpectedSuccess
  with
  | UnexpectedSuccess -> raise UnexpectedSuccess
  | _ -> ()

let read_compiler_error_tests admissible_fail directory =
  read_test_files directory
  |> List.map (compiler_error_test_case_from_file admissible_fail)

let compiler_error_tests label admissible_fail directory =
  label >::: read_compiler_error_tests admissible_fail directory

(** [read_all_lines f] reads all the lines in [f] in order. *)
let read_all_lines =
  let rec read_all_lines input_channel lines =
    try read_all_lines input_channel (input_line input_channel :: lines) with
    | End_of_file ->
      close_in input_channel;
      lines
    | e ->
      close_in_noerr input_channel;
      raise e
  in
  fun file -> List.rev @@ read_all_lines (open_in file) []

(** [read_admissible_fail_files project_root f] reads from [f] all the paths
    from [project_root] to files of test cases to skip. The paths in [f] are
    concatenated to [project_root]. *)
let read_admissible_fail_files project_root admissible_fails_file =
  read_all_lines admissible_fails_file
  |> List.to_seq
  |> Seq.filter_map
       Fun.(String.trim >> Option.from_predicate String.is_non_empty)
  |> Seq.map (Filename.concat project_root)
  |> String.Set.of_seq

let directory_from = Filename.concat

let file_from = Filename.concat

let examples_directory = Fun.(directory_from >> Fun.apply "./examples")

let code_success_directory =
  Fun.(directory_from >> Fun.apply "./t/code/success")

let code_failure_directory =
  Fun.(directory_from >> Fun.apply "./t/code/error")

let admissible_fail_file = Fun.(file_from >> Fun.apply "./.admissible-fail")

let () =
  Beluga.Chatter.level := 0;
  let cwd = Sys.getcwd () (* dirname of this file *) in
  let project_root = Filename.concat cwd "../.." in
  let admissible_fail =
    read_admissible_fail_files project_root
      (admissible_fail_file project_root)
  in
  run_test_tt_main
    ("Beluga"
    >::: [ compiler_success_tests "Examples" admissible_fail
             (examples_directory project_root)
         ; compiler_success_tests "Compiler Tests (Successes)"
             admissible_fail
             (code_success_directory project_root)
         ; compiler_error_tests "Compiler Tests (Errors)" admissible_fail
             (code_failure_directory project_root)
         ])
