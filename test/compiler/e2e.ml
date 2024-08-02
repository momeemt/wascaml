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
  let filename = find_project_root (Sys.getcwd ()) ^ "/test/compiler/tmp/" ^ test_name ^ ".wat" in
  let out_channel = open_out filename in
  output_string out_channel wat;
  close_out out_channel;
  let run_wasmtime filename =
    let command = Printf.sprintf "wasmtime %s" filename in
    let exit_status = Unix.system command in
    match exit_status with
    | WEXITED code -> code
    | WSIGNALED _ | WSTOPPED _ -> -1
  in
  run_wasmtime filename

let exec_int32_1 _ =
  let code = "42" in
  let result = exec_code code "int32_1" in
  Alcotest.(check int) code result 42

let exec_plus_1 _ =
  let code = "21 + 42" in
  let result = exec_code code "plus_1" in
  Alcotest.(check int) code result 63

let exec_minus_1 _ =
  let code = "40 - 15" in
  let result = exec_code code "minus_1" in
  Alcotest.(check int) code result 25

let exec_times_1 _ =
  let code = "8 * 5" in
  let result = exec_code code "times_1" in
  Alcotest.(check int) code result 40

let exec_div_1 _ =
  let code = "49 / 7" in
  let result = exec_code code "div_1" in
  Alcotest.(check int) code result 7

let exec_binops_1 _ =
  let code = "12 + 3 * 5" in
  let result = exec_code code "binops_1" in
  Alcotest.(check int) code result 27

let () =
  Alcotest.run "Compiler.e2e"
    [
      ("42", [ Alcotest.test_case "42" `Quick exec_int32_1 ]);
      ("21 + 42", [ Alcotest.test_case "21 + 42" `Quick exec_plus_1 ]);
      ("40 - 15", [ Alcotest.test_case "40 - 15" `Quick exec_minus_1 ]);
      ("8 * 5", [ Alcotest.test_case "8 * 5" `Quick exec_times_1 ]);
      ("49 / 7", [ Alcotest.test_case "49 / 7" `Quick exec_div_1 ]);
      ("12 + 3 * 5", [ Alcotest.test_case "12 + 3 * 5" `Quick exec_binops_1 ]);
    ]
