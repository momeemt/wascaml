type instr =
  | Block of string * expr (* 0x02 *)
  | Loop of string * expr (* 0x03 *)
  | If of expr * expr (* 0x04, 0x05 *)
  | IfVoid of expr * expr (* FIXME: hard code - result i32 *)
  | Br of string (* 0x0C *)
  | BrIf of string (* 0x0D *)
  | Call of string (* 0x10 *)
  | Drop (* 0x1A *)
  | LocalGet of string (* 0x20 *)
  | LocalSet of string (* 0x21 *)
  | LocalTee of string (* 0x22 *)
  | I32Load (* 0x28 *)
  | I32Load8U (* 0x2D *)
  | I32Store (* 0x36 *)
  | I32Store8 (* 0x3A *)
  | I32Const of int (* 0x41 *)
  | I32Eqz (* 0x45 *)
  | I32Eq (* 0x46 *)
  | I32LtU (* 0x49 *)
  | I32GtU (* 0x4B *)
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
  (* FIXME: hard code - result i32 *)
  | If (then_, else_) ->
      Printf.sprintf "(if (result i32)\n(then\n%s\n)\n(else\n%s\n)\n)"
        (string_of_exprs then_) (string_of_exprs else_)
  | IfVoid (then_, else_) ->
      Printf.sprintf "(if\n(then\n%s\n)\n(else\n%s\n)\n)"
        (string_of_exprs then_) (string_of_exprs else_)
  | Br label -> Printf.sprintf "br $%s" label
  | BrIf label -> Printf.sprintf "br_if $%s" label
  | Call label -> Printf.sprintf "call $%s" label
  | Drop -> Printf.sprintf "drop"
  | LocalGet label -> Printf.sprintf "local.get $%s" label
  | LocalSet label -> Printf.sprintf "local.set $%s" label
  | LocalTee label -> Printf.sprintf "local.tee $%s" label
  | I32Load -> "i32.load"
  | I32Load8U -> "i32.load8_u"
  | I32Store -> "i32.store"
  | I32Store8 -> "i32.store8"
  | I32Const v -> Printf.sprintf "i32.const %d" v
  | I32Eqz -> "i32.eqz"
  | I32Eq -> "i32.eq"
  | I32LtU -> "i32.lt_u"
  | I32GtU -> "i32.gt_u"
  | I32GeU -> "i32.ge_u"
  | I32Add -> "i32.add"
  | I32Sub -> "i32.sub"
  | I32Mul -> "i32.mul"
  | I32DivS -> "i32.div_s"
  | I32DivU -> "i32.div_u"
  | I32RemU -> "i32.rem_u"

and string_of_exprs exprs = String.concat "\n" (List.map string_of_instr exprs)

let string_of_expr expr = string_of_exprs expr
