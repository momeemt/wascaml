open Runtime.Modules
open Runtime.Instructions

let string_to_instrs str =
  let open List in
  let open Char in
  let open Runtime.Instructions in
  let start_addr = 0 in
  let new_func_body, _ =
    fold_left
      (fun (acc, addr) c ->
        (acc @ [ I32Const addr; I32Const (code c); I32Store ], addr + 1))
      ( [ I32Const start_addr; I32Const (String.length str); I32Store ],
        start_addr + 1 )
      (init (String.length str) (String.get str))
  in
  new_func_body @ [ I32Const start_addr ]

let int32_to_ascii =
  {
    name = "int32_to_ascii";
    params = [ (Some "num", I32); (Some "buffer", I32) ];
    results = [ I32 ];
    locals =
      [
        (Some "tmp", I32);
        (Some "pos", I32);
        (Some "start", I32);
        (Some "end", I32);
      ];
    body =
      [
        LocalGet "buffer";
        LocalSet "pos";
        Block
          ( "exit",
            [
              LocalGet "num";
              I32Eqz;
              IfVoid
                ( [
                    LocalGet "pos";
                    I32Const 48;
                    I32Store8;
                    LocalGet "pos";
                    I32Const 1;
                    I32Add;
                    LocalSet "pos";
                    Br "exit";
                  ],
                  [
                    Loop
                      ( "loop",
                        [
                          (* tmp = num % 10 *)
                          LocalGet "num";
                          I32Const 10;
                          I32RemU;
                          LocalSet "tmp";
                          (* *pos = tmp + 48 (to ascii) *)
                          LocalGet "pos";
                          LocalGet "tmp";
                          I32Const 48;
                          I32Add;
                          I32Store8;
                          (* pos++ *)
                          LocalGet "pos";
                          I32Const 1;
                          I32Add;
                          LocalSet "pos";
                          (* num /= 10 *)
                          LocalGet "num";
                          I32Const 10;
                          I32DivU;
                          LocalSet "num";
                          (* if num = 0 then break *)
                          LocalGet "num";
                          I32Eqz;
                          BrIf "exit";
                          (* to "loop" label *)
                          Br "loop";
                        ] );
                  ] );
            ] );
        (* *pos = 0 *)
        LocalGet "pos";
        I32Const 0;
        I32Store8;
        (* start = buffer *)
        LocalGet "buffer";
        LocalSet "start";
        (* end = pos *)
        LocalGet "pos";
        LocalSet "end";
        Block
          ( "reverse_exit",
            [
              Loop
                ( "reverse",
                  [
                    (* if start >= end then break *)
                    LocalGet "start";
                    LocalGet "end";
                    I32GeU;
                    BrIf "reverse_exit";
                    (* tmp = *start *)
                    LocalGet "start";
                    I32Load8U;
                    LocalSet "tmp";
                    (* *start = *(end - 1) *)
                    LocalGet "start";
                    LocalGet "end";
                    I32Const 1;
                    I32Sub;
                    I32Load8U;
                    I32Store8;
                    (* *(end - 1) = tmp *)
                    LocalGet "end";
                    I32Const 1;
                    I32Sub;
                    LocalGet "tmp";
                    I32Store8;
                    (* start += 1 *)
                    LocalGet "start";
                    I32Const 1;
                    I32Add;
                    LocalSet "start";
                    (* end -= 1 *)
                    LocalGet "end";
                    I32Const 1;
                    I32Sub;
                    LocalSet "end";
                    Br "reverse";
                  ] );
            ] );
        LocalGet "pos";
      ];
  }

let write_string name out =
  {
    name;
    params = [ (Some "str_addr", I32) ];
    locals = [ (Some "length", I32); (Some "buffer", I32) ];
    results = [];
    body =
      [
        LocalGet "str_addr";
        I32Const 1;
        I32Add;
        LocalSet "buffer";
        LocalGet "str_addr";
        I32Load8U;
        LocalSet "length";
        I32Const 100;
        LocalGet "buffer";
        I32Store;
        I32Const 104;
        LocalGet "length";
        I32Store;
        I32Const out;
        I32Const 100;
        I32Const 1;
        I32Const 108;
        Call "fd_write";
        Drop;
      ];
  }

let print_string = write_string "print_string" 1
let print_stderr_string = write_string "print_stderr_string" 2

let write_int32 name out =
  {
    name;
    params = [ (Some "num", I32) ];
    locals = [ (Some "buffer", I32); (Some "length", I32) ];
    results = [];
    body =
      [
        (* buffer = 64 *)
        I32Const 64;
        LocalSet "buffer";
        LocalGet "num";
        LocalGet "buffer";
        Call "int32_to_ascii";
        LocalSet "length";
        (* iov_base <- buffer *)
        I32Const 100;
        LocalGet "buffer";
        I32Store;
        (* iov_len <- length - buffer *)
        I32Const 104;
        LocalGet "length";
        LocalGet "buffer";
        I32Sub;
        I32Store;
        (* call fd_write *)
        I32Const out;
        I32Const 100;
        I32Const 1;
        I32Const 108;
        Call "fd_write";
        Drop;
        (* (1* add newline *1) *)
        (* I32Const 0; *)
        (* I32Const 10; *)
        (* I32Store; *)
        (* (1* iov_base <- buffer *1) *)
        (* I32Const 100; *)
        (* I32Const 0; *)
        (* I32Store; *)
        (* (1* iov_len <- length - buffer *1) *)
        (* I32Const 104; *)
        (* I32Const 1; *)
        (* I32Store; *)
        (* (1* call fd_write *1) *)
        (* I32Const out; *)
        (* I32Const 100; *)
        (* I32Const 1; *)
        (* I32Const 108; *)
        (* Call "fd_write"; *)
        (* Drop; *)
      ];
  }

let print_int32 = write_int32 "print_int32" 1
let print_stderr_int32 = write_int32 "print_stderr_int32" 2

let discard =
  {
    name = "discard";
    params = [ (Some "num", I32) ];
    locals = [];
    results = [];
    body = [ LocalGet "num"; Drop ];
  }

let list_hd =
  {
    name = "list_hd";
    params = [ (Some "lst_addr", I32) ];
    locals = [];
    results = [ I32 ];
    body = [ LocalGet "lst_addr"; I32Load ];
  }

let list_length =
  {
    name = "list_length";
    params = [ (Some "lst_addr", I32) ];
    locals = [ (Some "cnt", I32); (Some "buffer", I32) ];
    results = [ I32 ];
    body =
      [ I32Const 0; LocalSet "cnt"; LocalGet "lst_addr"; LocalSet "buffer" ]
      @ string_to_instrs "[PSan : list_length] => "
      @ [
          Call "print_stderr_string";
          LocalGet "buffer";
          Call "print_stderr_int32";
        ]
      @ string_to_instrs " "
      @ [ Call "print_stderr_string" ]
      @ [ LocalGet "lst_addr"; Call "print_stderr_list" ]
      @ string_to_instrs "\n"
      @ [ Call "print_stderr_string" ]
      @ [
          Block
            ( "exit",
              [
                Loop
                  ( "loop",
                    [
                      LocalGet "buffer";
                      I32Const (-1);
                      I32Eq;
                      BrIf "exit";
                      LocalGet "cnt";
                      I32Const 1;
                      I32Add;
                      LocalSet "cnt";
                      LocalGet "buffer";
                      I32Const 4;
                      I32Add;
                      I32Load;
                      LocalSet "buffer";
                      Br "loop";
                    ] );
              ] );
          LocalGet "cnt";
        ];
  }

let write_list name out =
  {
    name;
    params = [ (Some "lst_addr", I32) ];
    locals = [ (Some "buffer", I32) ];
    results = [];
    body =
      [ LocalGet "lst_addr"; LocalSet "buffer" (* output [ *) ]
      @ string_to_instrs "["
      @ [
          (if out = 1 then Call "print_string" else Call "print_stderr_string");
          Block
            ( "exit",
              [
                Loop
                  ( "loop",
                    [
                      LocalGet "buffer";
                      I32Const (-1);
                      I32Eq;
                      BrIf "exit";
                      LocalGet "buffer";
                      I32Load;
                      (if out = 1 then Call "print_int32"
                       else Call "print_stderr_int32");
                      LocalGet "buffer";
                      I32Const 4;
                      I32Add;
                      I32Load;
                      LocalSet "buffer";
                      LocalGet "buffer";
                      I32Const (-1);
                      I32Eq;
                      BrIf "exit";
                    ]
                    (* output ", " *) @ string_to_instrs ", "
                    @ [
                        (if out = 1 then Call "print_string"
                         else Call "print_stderr_string");
                        Br "loop";
                      ] );
              ] );
          (* output ] *)
        ]
      @ string_to_instrs "]"
      @ [
          (if out = 1 then Call "print_string" else Call "print_stderr_string");
        ];
  }

let print_list = write_list "print_list" 1
let print_stderr_list = write_list "print_stderr_list" 2
