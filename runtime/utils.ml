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
    body = [
      LocalGet "buffer";
      LocalSet "pos";
      Block ("exit", [
        Loop ("loop", [
          (* if num = 0 then break *)
          LocalGet "num";
          I32Eqz;
          BrIf "exit";
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
          (* to "loop" label *)
          Br "loop";
        ]);
      ]);
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
      Block ("reverse_exit", [
        Loop ("reverse", [
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
        ]);
      ]);
      LocalGet "pos";
    ];
  }

let print_int32 =
  {
    name = "print_int32";
    params = [ (Some "num", I32) ];
    locals = [(Some "buffer", I32); (Some "length", I32)];
    results = [];
    body = [
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
    ];
  }
