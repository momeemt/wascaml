type instr =
  | Block of string * expr (* 0x02 *)
  | Loop of string * expr (* 0x03 *)
  | Br of string (* 0x0C *)
  | BrIf of string (* 0x0D *)
  | Call of string (* 0x10 *)
  | Drop (* 0x1A *)
  | LocalGet of string (* 0x20 *)
  | LocalSet of string (* 0x21 *)
  | LocalTee of string (* 0x22 *)
  | I32Load8U (* 0x2D *)
  | I32Store (* 0x36 *)
  | I32Store8 (* 0x3A *)
  | I32Const of int (* 0x41 *)
  | I32Eqz (* 0x45 *)
  | I32GeU (* 0x4F *)
  | I32Add (* 0x6A *)
  | I32Sub (* 0x6B *)
  | I32Mul (* 0x6C *)
  | I32DivS (* 0x6D *)
  | I32DivU (* 0x6E *)
  | I32RemU (* 0x70 *)

and expr = instr list

let rec string_of_instr instr =
  match instr with
  | Block (label, expr) ->
      Printf.sprintf "(block $%s\n%s\n)" label (string_of_exprs expr)
  | Loop (label, expr) ->
      Printf.sprintf "(loop $%s\n%s\n)" label (string_of_exprs expr)
  | Br label -> Printf.sprintf "br $%s" label
  | BrIf label -> Printf.sprintf "br_if $%s" label
  | Call label -> Printf.sprintf "call $%s" label
  | Drop -> Printf.sprintf "drop"
  | LocalGet label -> Printf.sprintf "local.get $%s" label
  | LocalSet label -> Printf.sprintf "local.set $%s" label
  | LocalTee label -> Printf.sprintf "local.tee $%s" label
  | I32Load8U -> "i32.load8_u"
  | I32Store -> "i32.store"
  | I32Store8 -> "i32.store8"
  | I32Const v -> Printf.sprintf "i32.const %d" v
  | I32Eqz -> "i32.eqz"
  | I32GeU -> "i32.ge_u"
  | I32Add -> "i32.add"
  | I32Sub -> "i32.sub"
  | I32Mul -> "i32.mul"
  | I32DivS -> "i32.div_s"
  | I32DivU -> "i32.div_u"
  | I32RemU -> "i32.rem_u"

and string_of_exprs exprs =
  String.concat "\n" (List.map string_of_instr exprs)

let string_of_expr expr =
  string_of_exprs expr
