open Modules

let proc_exit =
  {
    module_name = "wasi_snapshot_preview1";
    name = "proc_exit";
    desc = FuncImport ("proc_exit", [ I32 ], []);
  }
