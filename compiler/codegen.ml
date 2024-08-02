open Ast
open Runtime.Instructions
open Runtime.Modules
open Runtime.Wasi

exception CodegenError of string

let rec codegen_expr expr =
  match expr with
  | IntLit n -> Instr (I32Const (Int32 n))
  | Plus (left, right) ->
      Block [ codegen_expr left; codegen_expr right; Instr I32Add ]
  | Minus (left, right) ->
      Block [ codegen_expr left; codegen_expr right; Instr I32Sub ]
  | Times (left, right) ->
      Block [ codegen_expr left; codegen_expr right; Instr I32Mul ]
  | Div (left, right) ->
      Block [ codegen_expr left; codegen_expr right; Instr I32DivS ]
  | _ -> raise (CodegenError "unsupported expr")

let codegen ast =
  let start_func =
    {
      name = "_start";
      params = [];
      results = [];
      body = [ codegen_expr ast; Instr (Call "proc_exit") ];
    }
  in
  let module_ =
    {
      imports = [ proc_exit ];
      exports = [ { name = "_start"; desc = FuncExport "_start" } ];
      funcs = [ start_func ];
      memory = Some { min_pages = 1; max_pages = None };
    }
  in
  string_of_module module_
