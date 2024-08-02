open Ast
open Runtime.Instructions
open Runtime.Modules
open Runtime.Utils
open Runtime.Wasi

exception CodegenError of string

let rec codegen_expr expr =
  match expr with
  | IntLit n -> [ I32Const n ]
  | If (cond, then_, else_) ->
      codegen_expr cond
      @ [ Runtime.Instructions.If (codegen_expr then_, codegen_expr else_) ]
  | Plus (left, right) -> codegen_expr left @ codegen_expr right @ [ I32Add ]
  | Minus (left, right) -> codegen_expr left @ codegen_expr right @ [ I32Sub ]
  | Times (left, right) -> codegen_expr left @ codegen_expr right @ [ I32Mul ]
  | Div (left, right) -> codegen_expr left @ codegen_expr right @ [ I32DivS ]
  | Eq (left, right) -> codegen_expr left @ codegen_expr right @ [ I32Eq ]
  | Greater (left, right) -> codegen_expr left @ codegen_expr right @ [ I32GtU ]
  | Less (left, right) -> codegen_expr left @ codegen_expr right @ [ I32LtU ]
  | _ -> raise (CodegenError "unsupported expr")

let codegen ast =
  let start_func =
    {
      name = "_start";
      params = [];
      results = [];
      body = codegen_expr ast @ [ Call "print_int32" ];
      locals = [];
    }
  in
  let module_ =
    {
      imports = [ fd_write; proc_exit ];
      exports = [ { name = "_start"; desc = FuncExport "_start" } ];
      funcs = [ int32_to_ascii; print_int32; start_func ];
      memory = Some { min_pages = 1; max_pages = None };
      data =
        [
          {
            offset = 0;
            value =
              "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
          };
        ];
    }
  in
  string_of_module module_
