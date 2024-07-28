let test_empty _ = Alcotest.(check bool) "true" true true

let () =
  Alcotest.run "Empty"
    [ ("empty", [ Alcotest.test_case "empty" `Quick test_empty ]) ]
