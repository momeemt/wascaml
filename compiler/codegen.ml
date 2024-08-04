open Ast
open Runtime.Instructions
open Runtime.Modules
open Runtime.Utils
open Runtime.Wasi

exception CodegenError of string

module Funcs = Map.Make (String)
module Env = Map.Make (String)

type identifierKind = Func | Arg

let codegen ast =
  let rec aux func_name funcs env expr =
    let aux_if cond then_ else_ =
      let cond_funcs = aux func_name funcs env cond in
      let cond = (Funcs.find func_name cond_funcs).body in
      let then_funcs = aux func_name cond_funcs env then_ in
      let then_ = (Funcs.find func_name then_funcs).body in
      let else_funcs = aux func_name then_funcs env else_ in
      let func = Funcs.find func_name else_funcs in
      let else_ = func.body in
      let new_func_body = cond @ [ Runtime.Instructions.If (then_, else_) ] in
      let new_func = { func with body = new_func_body } in
      Funcs.add func_name new_func else_funcs
    in
    let aux_binops left right op =
      let left_funcs = aux func_name funcs env left in
      let left = (Funcs.find func_name left_funcs).body in
      let right_funcs = aux func_name left_funcs env right in
      let func = Funcs.find func_name right_funcs in
      let right = func.body in
      let new_func_body = left @ right @ [ op ] in
      let new_func = { func with body = new_func_body } in
      Funcs.add func_name new_func right_funcs
    in
    let aux_let name params value body is_rec =
      let rand_name = name ^ "_" ^ string_of_int (Random.bits ()) in
      let funcs =
        Funcs.add rand_name
          {
            name = rand_name;
            params = List.map (fun param -> (Some param, I32)) params;
            results = [ I32 ];
            body = [];
            locals = [];
          }
          funcs
      in
      let value_funcs_env =
        List.fold_left
          (fun env param -> Env.add param (param, Arg) env)
          env params
      in
      let value_funcs_env =
        if is_rec then Env.add name (rand_name, Func) value_funcs_env
        else value_funcs_env
      in
      let funcs = aux rand_name funcs value_funcs_env value in
      aux func_name funcs (Env.add name (rand_name, Func) env) body
    in
    match expr with
    | IntLit n ->
        let func = Funcs.find func_name funcs in
        Funcs.add func_name { func with body = [ I32Const n ] } funcs
    | App (name, args) ->
        let func = Funcs.find func_name funcs in
        let funcs, args_instrs =
          List.fold_left
            (fun (funcs, acc) arg ->
              let funcs = aux func_name funcs env arg in
              let arg_instrs = (Funcs.find func_name funcs).body in
              (funcs, acc @ arg_instrs))
            (funcs, []) args
        in
        let wat_name, wat_kind =
          match Env.find_opt name env with
          | Some t -> t
          | None -> raise (CodegenError ("not found identifier: " ^ name))
        in
        let new_func_body =
          match wat_kind with
          | Func -> args_instrs @ [ Call wat_name ]
          | Arg -> args_instrs @ [ LocalGet wat_name ]
        in
        Funcs.add func_name { func with body = new_func_body } funcs
    | Sequence exprs ->
        let func = Funcs.find func_name funcs in
        let funcs, exprs_instrs =
          List.fold_left
            (fun (funcs, acc) expr ->
              let funcs = aux func_name funcs env expr in
              let expr_instrs = (Funcs.find func_name funcs).body in
              let drop_instrs = if expr <> List.hd (List.rev exprs) then [Drop] else [] in
              (funcs, acc @ expr_instrs @ drop_instrs))
            (funcs, []) exprs
        in
        Funcs.add func_name { func with body = exprs_instrs } funcs
    | If (cond, then_, else_) -> aux_if cond then_ else_
    | Let (name, params, value, body) -> aux_let name params value body false
    | LetRec (name, params, value, body) -> aux_let name params value body true
    | Plus (left, right) -> aux_binops left right I32Add
    | Minus (left, right) -> aux_binops left right I32Sub
    | Times (left, right) -> aux_binops left right I32Mul
    | Div (left, right) -> aux_binops left right I32DivS
    | Eq (left, right) -> aux_binops left right I32Eq
    | Greater (left, right) -> aux_binops left right I32GtU
    | Less (left, right) -> aux_binops left right I32LtU
    | rest ->
        raise
          (CodegenError
             (("unsupported expr\n" ^ string_of_ast rest)
             ^ "\n" ^ string_of_ast ast))
  in
  let start_func =
    { name = "_start"; params = []; results = []; body = []; locals = [] }
  in
  let funcs =
    aux "_start" Funcs.(empty |> add "_start" start_func) Env.empty ast
  in
  let funcs =
    Funcs.map
      (fun (fn : func) ->
        if fn.name = "_start" then
          { fn with body = fn.body @ [ Call "print_int32" ] }
        else fn)
      funcs
  in
  let module_ =
    {
      imports = [ fd_write; proc_exit ];
      exports = [ { name = "_start"; desc = FuncExport "_start" } ];
      funcs =
        [ int32_to_ascii; print_int32 ] @ (Funcs.bindings funcs |> List.map snd);
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
