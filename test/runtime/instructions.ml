open Runtime.Instructions

let test_string_of_expr_i32_const _ =
  Alcotest.(check string)
    "string_of_expr (i32.const 42)"
    ([ I32Const 42 ] |> string_of_expr)
    "i32.const 42"

let test_string_of_expr_i32_add _ =
  Alcotest.(check string)
    "string_of_expr (i32.add)"
    ([ I32Add ] |> string_of_expr)
    "i32.add"

let test_string_of_expr_i32_const_and_add _ =
  Alcotest.(check string)
    "string_of_expr [ (i32.const 42); (i32.const 21); (i32.add) ]"
    ([ I32Const 42; I32Const 21; I32Add ] |> string_of_expr)
    "i32.const 42\ni32.const 21\ni32.add"

let () =
  Alcotest.run "Runtime.Instructions"
    [
      ( "string_of_expr_i32_const",
        [
          Alcotest.test_case "string_of_expr_i32_const" `Quick
            test_string_of_expr_i32_const;
        ] );
      ( "string_of_expr_i32_add",
        [
          Alcotest.test_case "string_of_expr_i32_add" `Quick
            test_string_of_expr_i32_add;
        ] );
      ( "string_of_expr_i32_const_and_add",
        [
          Alcotest.test_case "string_of_expr_i32_const_and_add" `Quick
            test_string_of_expr_i32_const_and_add;
        ] );
    ]
