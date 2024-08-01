open Runtime.Value

let test_string_of_value_i32 _ =
  Alcotest.(check string)
    "string_of_value (22 :: Int32)"
    (Int32 22 |> string_of_value)
    "22"

let test_string_of_value_i64 _ =
  Alcotest.(check string)
    "string_of_value (22 :: Int64)"
    (Int64 22 |> string_of_value)
    "22"

let test_string_of_value_f32 _ =
  Alcotest.(check string)
    "string_of_value (43.21 :: Float32)"
    (Float32 43.21 |> string_of_value)
    "43.21"

let test_string_of_value_f64 _ =
  Alcotest.(check string)
    "string_of_value (43.21 :: Float64)"
    (Float64 43.21 |> string_of_value)
    "43.21"

let () =
  Alcotest.run "Runtime.Value"
    [
      ( "string_of_value_i32",
        [
          Alcotest.test_case "string_of_value_i32" `Quick
            test_string_of_value_i32;
        ] );
      ( "string_of_value_i64",
        [
          Alcotest.test_case "string_of_value_i64" `Quick
            test_string_of_value_i64;
        ] );
      ( "string_of_value_f32",
        [
          Alcotest.test_case "string_of_value_f32" `Quick
            test_string_of_value_f32;
        ] );
      ( "string_of_value_f64",
        [
          Alcotest.test_case "string_of_value_f64" `Quick
            test_string_of_value_f64;
        ] );
    ]
