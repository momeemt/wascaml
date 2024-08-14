open Ast
open Builtin
open Inferer
open Runtime.Instructions
open Runtime.Modules
open Runtime.Wasi

exception CodegenError of string

module Funcs = Map.Make (String)
module Env = Map.Make (String)

type identifierKind = Func | Arg

let codegen ast te =
  let rec aux func_name funcs env expr addr =
    let aux_if cond then_ else_ addr =
      let cond_funcs, addr = aux func_name funcs env cond addr in
      let cond = (Funcs.find func_name cond_funcs).body in
      let then_funcs, addr = aux func_name cond_funcs env then_ addr in
      let then_ = (Funcs.find func_name then_funcs).body in
      let else_funcs, addr = aux func_name then_funcs env else_ addr in
      let func = Funcs.find func_name else_funcs in
      let else_ = func.body in
      let new_func_body = cond @ [ Runtime.Instructions.If (then_, else_) ] in
      let new_func = { func with body = new_func_body } in
      (Funcs.add func_name new_func else_funcs, addr)
    in
    let aux_binops left right op addr =
      let left_funcs, addr = aux func_name funcs env left addr in
      let left = (Funcs.find func_name left_funcs).body in
      let right_funcs, addr = aux func_name left_funcs env right addr in
      let func = Funcs.find func_name right_funcs in
      let right = func.body in
      let new_func_body = left @ right @ [ op ] in
      let new_func = { func with body = new_func_body } in
      (Funcs.add func_name new_func right_funcs, addr)
    in
    let aux_let name params value body is_rec addr =
      let rand_name = name ^ "_" ^ string_of_int (Random.bits ()) in
      let funcs =
        Funcs.add rand_name
          {
            name = rand_name;
            params = List.map (fun param -> (Some param, I32)) params;
            results =
              (let ty = lookup name te in
               match ty with TUnit -> [] | _ -> [ I32 ]);
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
      let funcs, addr = aux rand_name funcs value_funcs_env value addr in
      aux func_name funcs (Env.add name (rand_name, Func) env) body addr
    in
    match expr with
    | IntLit n ->
        let func = Funcs.find func_name funcs in
        (Funcs.add func_name { func with body = [ I32Const n ] } funcs, addr)
    | StringLit s ->
        let func = Funcs.find func_name funcs in
        let start_addr = addr in
        let new_func_body, addr =
          List.fold_left
            (fun (acc, addr) c ->
              ( acc @ [ I32Const addr; I32Const (Char.code c); I32Store ],
                addr + 1 ))
            ([ I32Const addr; I32Const (String.length s); I32Store ], addr + 1)
            (List.init (String.length s) (String.get s))
        in
        let new_func_body = new_func_body @ [ I32Const start_addr ] in
        (Funcs.add func_name { func with body = new_func_body } funcs, addr)
    | List lst ->
        let func = Funcs.find func_name funcs in
        let funcs, lst_instrs, end_addr =
          List.fold_left
            (fun (funcs, acc, addr) (expr, is_last) ->
              let funcs, addr = aux func_name funcs env expr addr in
              let expr_instrs = (Funcs.find func_name funcs).body in
              let next_addr = addr + 4 in
              let load_next_instrs =
                if is_last then [ I32Const (-1) ]
                else [ I32Const (next_addr + 4) ]
              in
              let store_instrs =
                [ I32Const addr ] @ expr_instrs
                @ [ I32Store; I32Const next_addr ]
                @ load_next_instrs @ [ I32Store ]
              in
              (funcs, acc @ store_instrs, next_addr + 4))
            (funcs, [], addr)
            (List.mapi (fun i expr -> (expr, i = List.length lst - 1)) lst)
        in
        let head_addr = if List.length lst = 0 then -1 else addr in
        let new_func_body = lst_instrs @ [ I32Const head_addr ] in
        (Funcs.add func_name { func with body = new_func_body } funcs, end_addr)
    | Cons (cons, lst) ->
        let func = Funcs.find func_name funcs in
        let lst_funcs, addr = aux func_name funcs env lst addr in
        let lst_expr_instr = (Funcs.find func_name lst_funcs).body in
        let cons_funcs, addr = aux func_name lst_funcs env cons addr in
        let cons_expr_instr = (Funcs.find func_name cons_funcs).body in
        let new_func_body =
          [ I32Const addr ] @ cons_expr_instr @ [ I32Store ]
        in
        let next_addr = addr + 4 in
        let new_func_body =
          new_func_body @ [ I32Const next_addr ] @ lst_expr_instr @ [ I32Store ]
          @ string_to_instrs "[PSan : cons] => "
          @ [ Call "print_stderr_string" ]
          @ [ I32Const addr; Call "print_stderr_list" ]
          @ string_to_instrs "\n"
          @ [ Call "print_stderr_string"; I32Const addr ]
        in
        ( Funcs.add func_name { func with body = new_func_body } funcs,
          next_addr + 4 )
    | Append (lst1, lst2) ->
        let func = Funcs.find func_name funcs in
        let lst1_funcs, addr = aux func_name funcs env lst1 addr in
        let lst1_expr_instr = (Funcs.find func_name lst1_funcs).body in
        let lst2_funcs, addr = aux func_name lst1_funcs env lst2 addr in
        let lst2_expr_instr = (Funcs.find func_name lst2_funcs).body in
        let copy_lst1_instrs, new_addr =
          let rec aux acc addr i instrs =
            match instrs with
            | [] -> (acc, addr)
            | instr :: instrs ->
                if i = 0 || i = 3 then
                  aux (I32Const addr :: acc) (addr + 4) (i + 1) instrs
                else if i = 4 then
                  aux (I32Const addr :: acc) addr (i + 1) instrs
                else if i = 5 then aux (instr :: acc) addr 0 instrs
                else aux (instr :: acc) addr (i + 1) instrs
          in
          aux [] addr 0 lst1_expr_instr
        in
        let copy_lst1_instrs = List.tl copy_lst1_instrs in
        let copy_lst2_instrs, _ =
          let rec aux acc addr i instrs =
            match instrs with
            | [] -> (acc, addr)
            | instr :: instrs ->
                if i = 0 || i = 3 then
                  aux (I32Const addr :: acc) (addr + 4) (i + 1) instrs
                else if i = 4 && instr <> I32Const (-1) then
                  aux (I32Const addr :: acc) addr (i + 1) instrs
                else if i = 5 then aux (instr :: acc) addr 0 instrs
                else aux (instr :: acc) addr (i + 1) instrs
          in
          aux [] (new_addr - 4) 0 lst2_expr_instr
        in
        let copy_lst2_instrs = List.tl copy_lst2_instrs in
        let new_func_body =
          lst1_expr_instr @ [ Drop ] @ lst2_expr_instr @ [ Drop ]
          @ List.rev copy_lst1_instrs @ List.rev copy_lst2_instrs
          @ [ I32Const addr ]
        in
        (Funcs.add func_name { func with body = new_func_body } lst2_funcs, addr)
    | App (name, args) ->
        let func = Funcs.find func_name funcs in
        let funcs, args_instrs, end_addr =
          List.fold_left
            (fun (funcs, acc, addr) arg ->
              let funcs, addr = aux func_name funcs env arg addr in
              let arg_instrs = (Funcs.find func_name funcs).body in
              (funcs, acc @ arg_instrs, addr))
            (funcs, [], addr) args
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
        (Funcs.add func_name { func with body = new_func_body } funcs, end_addr)
    | Sequence exprs ->
        let func = Funcs.find func_name funcs in
        let funcs, exprs_instrs, end_addr =
          List.fold_left
            (fun (funcs, acc, addr) expr ->
              let funcs, addr = aux func_name funcs env expr addr in
              let expr_instrs = (Funcs.find func_name funcs).body in
              (funcs, acc @ expr_instrs, addr))
            (funcs, [], addr) exprs
        in
        (Funcs.add func_name { func with body = exprs_instrs } funcs, end_addr)
    | If (cond, then_, else_) -> aux_if cond then_ else_ addr
    | Let (name, params, value, body) ->
        aux_let name params value body false addr
    | LetRec (name, params, value, body) ->
        aux_let name params value body true addr
    | Plus (left, right) -> aux_binops left right I32Add addr
    | Minus (left, right) -> aux_binops left right I32Sub addr
    | Times (left, right) -> aux_binops left right I32Mul addr
    | Div (left, right) -> aux_binops left right I32DivS addr
    | Eq (left, right) -> aux_binops left right I32Eq addr
    | Greater (left, right) -> aux_binops left right I32GtU addr
    | Less (left, right) -> aux_binops left right I32LtU addr
    | rest ->
        raise
          (CodegenError
             (("unsupported expr\n" ^ string_of_ast rest)
             ^ "\n" ^ string_of_ast ast))
  in
  let start_func =
    { name = "_start"; params = []; results = []; body = []; locals = [] }
  in
  let env =
    Env.(
      empty
      |> add "print_int32" ("print_int32", Func)
      |> add "print_stderr_int32" ("print_stderr_int32", Func)
      |> add "print_list" ("print_list", Func)
      |> add "print_stderr_list" ("print_stderr_list", Func)
      |> add "print_string" ("print_string", Func)
      |> add "print_stderr_string" ("print_stderr_string", Func)
      |> add "list_length" ("list_length", Func)
      |> add "list_hd" ("list_hd", Func)
      |> add "list_next" ("list_next", Func)
      |> add "list_copy" ("list_copy", Func)
      |> add "discard" ("discard", Func))
  in
  let funcs, _ =
    aux "_start" Funcs.(empty |> add "_start" start_func) env ast 1024
  in
  let funcs =
    Funcs.map
      (fun (fn : func) ->
        if fn.name = "_start" then { fn with body = fn.body } else fn)
      funcs
  in
  let module_ =
    {
      imports = [ fd_write; proc_exit ];
      exports = [ { name = "_start"; desc = FuncExport "_start" } ];
      funcs =
        [
          int32_to_ascii;
          print_int32;
          print_stderr_int32;
          print_list;
          print_stderr_list;
          print_string;
          print_stderr_string;
          list_length;
          list_hd;
          list_next;
          list_copy;
          discard;
        ]
        @ (Funcs.bindings funcs |> List.map snd);
      memory = Some { min_pages = 1; max_pages = None };
      data = [];
    }
  in
  string_of_module module_
