open Modules
open Instructions

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

let print_string =
  {
    name = "print_string";
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
        I32Const 1;
        I32Const 100;
        I32Const 1;
        I32Const 108;
        Call "fd_write";
        Drop;
        I32Const 0;
        I32Const 10;
        I32Store;
        I32Const 100;
        I32Const 0;
        I32Store;
        I32Const 104;
        I32Const 1;
        I32Store;
        I32Const 1;
        I32Const 100;
        I32Const 1;
        I32Const 108;
        Call "fd_write";
        Drop;
      ];
  }

let print_int32 =
  {
    name = "print_int32";
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
        I32Const 1;
        I32Const 100;
        I32Const 1;
        I32Const 108;
        Call "fd_write";
        Drop;
        (* add newline *)
        I32Const 0;
        I32Const 10;
        I32Store;
        (* iov_base <- buffer *)
        I32Const 100;
        I32Const 0;
        I32Store;
        (* iov_len <- length - buffer *)
        I32Const 104;
        I32Const 1;
        I32Store;
        (* call fd_write *)
        I32Const 1;
        I32Const 100;
        I32Const 1;
        I32Const 108;
        Call "fd_write";
        Drop;
      ];
  }

let discard =
  {
    name = "discard";
    params = [ (Some "num", I32) ];
    locals = [];
    results = [];
    body = [ LocalGet "num"; Drop ];
  }

let list_length =
  {
    name = "list_length";
    params = [ (Some "lst_addr", I32) ];
    locals = [];
    results = [ I32 ];
    body = [ LocalGet "lst_addr"; I32Load8U ];
  }

let print_list =
  {
    name = "print_list";
    params = [ (Some "lst_addr", I32) ];
    locals = [ (Some "length", I32); (Some "buffer", I32) ];
    results = [];
    body =
      [
        LocalGet "lst_addr";
        I32Load8U;
        LocalSet "length";
        LocalGet "lst_addr";
        I32Const 1;
        I32Add;
        LocalSet "buffer";
        Block
          ( "exit",
            [
              Loop
                ( "loop",
                  [
                    LocalGet "length";
                    I32Eqz;
                    BrIf "exit";
                    LocalGet "buffer";
                    I32Load8U;
                    Call "print_int32";
                    LocalGet "buffer";
                    I32Const 1;
                    I32Add;
                    LocalSet "buffer";
                    LocalGet "length";
                    I32Const 1;
                    I32Sub;
                    LocalSet "length";
                    Br "loop";
                  ] );
            ] );
      ];
  }
