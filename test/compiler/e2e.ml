open Compiler.Codegen
open Compiler.Parser
open Compiler.Tokenizer
open Filename

let exec_code code =
  let tokens = tokenize code in
  let ast = parse tokens in
  let wat = codegen ast in
  let filename, out_channel = open_temp_file "" "" in
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
  let result = exec_code code in
  Alcotest.(check int) code result 42

let exec_plus_1 _ =
  let code = "21 + 42" in
  let result = exec_code code in
  Alcotest.(check int) code result 63

let exec_minus_1 _ =
  let code = "40 - 15" in
  let result = exec_code code in
  Alcotest.(check int) code result 25

let exec_times_1 _ =
  let code = "8 * 5" in
  let result = exec_code code in
  Alcotest.(check int) code result 40

let exec_div_1 _ =
  let code = "49 / 7" in
  let result = exec_code code in
  Alcotest.(check int) code result 7

let exec_binops_1 _ =
  let code = "12 + 3 * 5" in
  let result = exec_code code in
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
