open Compiler.Ast
open Compiler.Types

let test_string_of_ast_int_lit _ =
  Alcotest.(check string)
    "string_of_ast (42)"
    (IntLit (TInt, 42) |> string_of_ast)
    "IntLit (42) : int"

let test_string_of_ast_plus _ =
  Alcotest.(check string)
    "string_of_ast (28 + 42)"
    (Plus (TInt, (IntLit (TInt, 28)), IntLit (TInt, 42)) |> string_of_ast)
    "Plus (IntLit (28) : int, IntLit (42) : int) : int"

let test_string_of_ast_minus _ =
  Alcotest.(check string)
    "string_of_ast (42 - 28)"
    (Minus (TInt, (IntLit (TInt, 42)), IntLit (TInt, 28)) |> string_of_ast)
    "Minus (IntLit (42) : int, IntLit (28) : int) : int"

let test_string_of_ast_times _ =
  Alcotest.(check string)
    "string_of_ast (8 * 5)"
    (Times (TInt, (IntLit (TInt, 8)), IntLit (TInt, 5)) |> string_of_ast)
    "Times (IntLit (8) : int, IntLit (5) : int) : int"

let test_string_of_ast_div _ =
  Alcotest.(check string)
    "string_of_ast (12 / 4)"
    (Div (TInt, (IntLit (TInt, 12)), IntLit (TInt, 4)) |> string_of_ast)
    "Div (IntLit (12) : int, IntLit (4) : int) : int"

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
