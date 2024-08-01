open Compiler.Ast

let test_string_of_ast_int_lit _ =
  Alcotest.(check string)
    "string_of_ast (42)"
    (IntLit 42 |> string_of_ast)
    "IntLit (42)"

let test_string_of_ast_plus _ =
  Alcotest.(check string)
    "string_of_ast (28 + 42)"
    (Plus (IntLit 28, IntLit 42) |> string_of_ast)
    "Plus (IntLit (28), IntLit (42))"

let test_string_of_ast_minus _ =
  Alcotest.(check string)
    "string_of_ast (42 - 28)"
    (Minus (IntLit 42, IntLit 28) |> string_of_ast)
    "Minus (IntLit (42), IntLit (28))"

let test_string_of_ast_times _ =
  Alcotest.(check string)
    "string_of_ast (8 * 5)"
    (Times (IntLit 8, IntLit 5) |> string_of_ast)
    "Times (IntLit (8), IntLit (5))"

let test_string_of_ast_div _ =
  Alcotest.(check string)
    "string_of_ast (12 / 4)"
    (Div (IntLit 12, IntLit 4) |> string_of_ast)
    "Div (IntLit (12), IntLit (4))"

let () =
  Alcotest.run "Compiler.Ast"
    [
      ( "string_of_ast_int_lit",
        [
          Alcotest.test_case "string_of_ast_int_lit" `Quick
            test_string_of_ast_int_lit;
        ] );
      ( "string_of_ast_plus",
        [
          Alcotest.test_case "string_of_ast_plus" `Quick test_string_of_ast_plus;
        ] );
      ( "string_of_ast_minus",
        [
          Alcotest.test_case "string_of_ast_minus" `Quick
            test_string_of_ast_minus;
        ] );
      ( "string_of_ast_times",
        [
          Alcotest.test_case "string_of_ast_times" `Quick
            test_string_of_ast_times;
        ] );
      ( "string_of_ast_div",
        [ Alcotest.test_case "string_of_ast_div" `Quick test_string_of_ast_div ]
      );
    ]
