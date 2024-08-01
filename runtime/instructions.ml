open Value

type instr = I32Const of value | I32Add | I32Sub | I32Mul | I32DivS
and expr = Instr of instr | Block of expr list

let rec string_of_instr instr =
  match instr with
  | I32Const v -> Printf.sprintf "i32.const %s" (string_of_value v)
  | I32Add -> "i32.add"
  | I32Sub -> "i32.sub"
  | I32Mul -> "i32.mul"
  | I32DivS -> "i32.div_s"

and string_of_expr expr =
  match expr with
  | Instr instr -> string_of_instr instr
  | Block exprs ->
      Printf.sprintf "%s" (List.map string_of_expr exprs |> String.concat "\n")
