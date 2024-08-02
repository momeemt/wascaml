open Modules

let wasi_snapshot_preview1 = "wasi_snapshot_preview1"

let proc_exit =
  {
    module_name = wasi_snapshot_preview1;
    name = "proc_exit";
    desc = FuncImport ("proc_exit", [ I32 ], []);
  }

let fd_write =
  {
    module_name = wasi_snapshot_preview1;
    name = "fd_write";
    desc = FuncImport ("fd_write", [ I32; I32; I32; I32 ], [ I32 ]);
  }
