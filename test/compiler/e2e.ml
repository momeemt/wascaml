open Compiler.Codegen
open Compiler.Parser
open Compiler.Tokenizer
open Compiler.Inferer

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
  let te, _, _, _ = tinf ast in
  let wat = codegen ast te in
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

let exec_code_test case_name code expected =
  let result = exec_code code case_name in
  Alcotest.(check string) code expected result

let test_case name code expected_result =
  Alcotest.test_case code `Quick (fun _ ->
      exec_code_test name code (string_of_int expected_result ^ "\n"))

let test_case_str name code expected =
  Alcotest.test_case code `Quick (fun _ -> exec_code_test name code expected)

let () =
  Alcotest.run "Compiler.e2e"
    [
      ("int32_1", [ test_case "int32_1" "print_int32 42" 42 ]);
      ("int32_2", [ test_case "int32_2" "print_int32 0" 0 ]);
      ("plus_1", [ test_case "plus_1" "print_int32 (21 + 42)" 63 ]);
      ("minus_1", [ test_case "minus_1" "print_int32 (40 - 15)" 25 ]);
      ("times_1", [ test_case "times_1" "print_int32 (8 * 5)" 40 ]);
      ("div_1", [ test_case "div_1" "print_int32 (49 / 7)" 7 ]);
      ("binops_1", [ test_case "binops_1" "print_int32 (12 + 3 * 5)" 27 ]);
      ("if_1", [ test_case "if_1" "print_int32 (if 0 = 0 then 1 else 3)" 1 ]);
      ("if_2", [ test_case "if_2" "print_int32 (if 1 = 2 then 3 else 4)" 4 ]);
      ("if_3", [ test_case "if_3" "print_int32 (if 1 < 10 then 20 else 30)" 20 ]);
      ("if_4", [ test_case "if_4" "print_int32 (if 2 > 20 then 40 else 50)" 50 ]);
      ("let_1", [ test_case "let_1" "let x = 10 in print_int32 x" 10 ]);
      ("let_2", [ test_case "let_2" "let x = 3 in print_int32 (2 + x * x)" 11 ]);
      ("let_3", [ test_case "let_3" "let f x = x + x in print_int32 (f 3)" 6 ]);
      ( "let_4",
        [ test_case "let_4" "let f x y = x * y in print_int32 (f 11 13)" 143 ]
      );
      ( "let_5",
        [
          test_case "let_5"
            "let f x = let g y = y + y in g x in print_int32 (f 3)" 6;
        ] );
      ( "let_6",
        [
          test_case "let_6"
            "let f x = let f y = y * y in f (x + x) in print_int32 (f 3)" 36;
        ] );
      ( "let_7",
        [
          test_case "let_7"
            "let f x = let g x = x + x in g x + (g x) in print_int32 (f 10)" 40;
        ] );
      ( "let_8",
        [
          test_case "let_8"
            "let f x = let f x = x * x in f x * (f x) in print_int32 (f 2)" 16;
        ] );
      ( "let_9",
        [ test_case "let_9" "let f1 x1 = x1 * x1 in print_int32 (f1 10)" 100 ]
      );
      ( "let_10",
        [
          test_case "let_10" "let f_1 x_1 = x_1 * x_1 in print_int32 (f_1 10)"
            100;
        ] );
      ( "let_rec_1",
        [
          test_case "let_rec_1"
            "let rec fact x = if x = 0 then 1 else x * (fact (x - 1)) in \
             print_int32 (fact 5)"
            120;
        ] );
      ( "sequence_1",
        [ test_case "sequence_1" "discard 1; discard 2; print_int32 3" 3 ] );
      ( "sequence_2",
        [
          test_case_str "sequence_2"
            "print_int32 (if 1 = 2 then 3 else 4); print_string \", \"; \
             print_int32 (let x = 10 in x + 20); print_string \", \"; \
             print_int32 (3 * 8)"
            "4, 30, 24\n";
        ] );
      ( "list_1",
        [ test_case_str "list_1" "print_list [1 2 3 4 5]" "[1, 2, 3, 4, 5]\n" ]
      );
      ( "list_length_1",
        [ test_case "list_length_1" "print_int32 (list_length [10 20 30])" 3 ]
      );
      ( "list_length_2",
        [ test_case "list_length_2" "print_int32 (list_length [])" 0 ] );
      ( "list_cons_1",
        [
          test_case_str "list_cons_1" "print_list (1 :: [2 3 4 5])"
            "[1, 2, 3, 4, 5]\n";
        ] );
      ( "list_cons_2",
        [
          test_case_str "list_cons_2" "print_list (1 :: 2 :: [3])" "[1, 2, 3]\n";
        ] );
      ( "list_cons_3",
        [
          test_case_str "list_cons_3" "print_list (1 :: 2 :: 3 :: [])"
            "[1, 2, 3]\n";
        ] );
      ("list_hd_1", [ test_case "list_hd_1" "print_int32 (list_hd [1 2 3])" 1 ]);
      ( "list_hd_2",
        [
          test_case "list_hd_2"
            "let head lst = list_hd lst in print_int32 (head [10 11 12])" 10;
        ] );
      ( "list_next_1",
        [
          test_case "list_next_1" "print_int32 (list_hd (list_next [1 2 3]))" 2;
        ] );
      ( "list_next_2",
        [
          test_case "list_next_2"
            "let rec get x lst = if x = 0 then list_hd lst else get (x-1) \
             (list_next lst) in print_int32 (get 3 [1 2 3 4])"
            4;
        ] );
      ( "list_append_1", [
        test_case_str "list_append_1" "print_list ([1 2] @ [3 4])" "[1, 2, 3, 4]\n"
      ] );
      ("list_append_2", [
        test_case_str "list_append_2" "let x = [1 2] in print_list (x @ [3 4])" "[1, 2, 3, 4]\n"
      ]);
      ( "string_1",
        [
          test_case_str "string_1" "print_string \"Hello, World!\""
            "Hello, World!\n";
        ] );
      ( "string_2",
        [
          test_case_str "string_2"
            "print_string \"# Todo\n\
             \"; print_string \"- File I/O\n\
             \"; print_string \"- ref\"" "# Todo\n- File I/O\n- ref\n";
        ] );
    ]
