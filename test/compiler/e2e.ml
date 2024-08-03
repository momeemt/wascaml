open Compiler.Codegen
open Compiler.Parser
open Compiler.Tokenizer

let rec find_project_root current_dir =
  let dune_project_path = Filename.concat current_dir "dune-project" in
  if Sys.file_exists dune_project_path then current_dir
  else
    let parent_dir = Filename.dirname current_dir in
    if String.equal current_dir parent_dir then
      failwith "Project root not found"
    else find_project_root parent_dir

let exec_code code test_name =
  let tokens = tokenize code in
  let ast = parse tokens in
  let wat = codegen ast in
  let filename =
    find_project_root (Sys.getcwd ())
    ^ "/test/compiler/tmp/" ^ test_name ^ ".wat"
  in
  let out_channel = open_out filename in
  output_string out_channel wat;
  close_out out_channel;
  let run_wasmtime filename =
    let command = Printf.sprintf "wasmtime %s" filename in
    let in_channel = Unix.open_process_in command in
    let rec read_all_lines acc =
      try
        let line = input_line in_channel in
        read_all_lines (acc ^ line ^ "\n")
      with End_of_file -> acc
    in
    let output = read_all_lines "" in
    let _ = Unix.close_process_in in_channel in
    output
  in
  run_wasmtime filename

let exec_code_test case_name code expected_result =
  let result = exec_code code case_name in
  Alcotest.(check string) code result (string_of_int expected_result ^ "\n")

let test_case name code expected_result =
  Alcotest.test_case code `Quick (fun _ ->
      exec_code_test name code expected_result)

let () =
  Alcotest.run "Compiler.e2e"
    [
      ("int32_1", [ test_case "int32_1" "42" 42 ]);
      ("plus_1", [ test_case "plus_1" "21 + 42" 63 ]);
      ("minus_1", [ test_case "minus_1" "40 - 15" 25 ]);
      ("times_1", [ test_case "times_1" "8 * 5" 40 ]);
      ("div_1", [ test_case "div_1" "49 / 7" 7 ]);
      ("binops_1", [ test_case "binops_1" "12 + 3 * 5" 27 ]);
      ("if_1", [ test_case "if_1" "if 0 = 0 then 1 else 2" 1 ]);
      ("if_2", [ test_case "if_2" "if 1 = 2 then 3 else 4" 4 ]);
      ("if_3", [ test_case "if_3" "if 1 < 10 then 20 else 30" 20 ]);
      ("if_4", [ test_case "if_4" "if 2 > 20 then 40 else 50" 50 ]);
      ("let_1", [ test_case "let_1" "let x = 10 in x" 10 ]);
      ("let_2", [ test_case "let_2" "let x = 3 in 2 + x * x" 11 ]);
      ("let_3", [ test_case "let_3" "let f x = x + x in f 3" 6 ]);
      ("let_4", [ test_case "let_4" "let f x y = x * y in f 11 13" 143 ]);
      ( "let_5",
        [
          test_case "let_5"
            "let fact x = if x = 0 then 1 else x * (fact (x - 1)) in fact 5" 120;
        ] );
    ]
