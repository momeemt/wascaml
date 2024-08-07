type ast =
  | Let of string * string list * ast * ast
  | LetRec of string * string list * ast * ast
  | Fun of string * ast
  | App of string * ast list
  | Sequence of ast list
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  | BoolLit of bool
  | List of ast list
  | If of ast * ast * ast
  | Eq of ast * ast
  | Less of ast * ast
  | Greater of ast * ast
  | Plus of ast * ast
  | Minus of ast * ast
  | Times of ast * ast
  | Div of ast * ast

let rec string_of_ast ast =
  match ast with
  | Let (id, args, e1, e2) ->
      "Let (" ^ id ^ ", [" ^ String.concat "; " args ^ "], " ^ string_of_ast e1
      ^ ", " ^ string_of_ast e2 ^ ")"
  | LetRec (f, args, e1, e2) ->
      "LetRec (" ^ f ^ ", [" ^ String.concat "; " args ^ "], "
      ^ string_of_ast e1 ^ ", " ^ string_of_ast e2 ^ ")"
  | Fun (id, body) -> "Fun (" ^ id ^ ", " ^ string_of_ast body ^ ")"
  | App (name, exprs) ->
      "App (" ^ name ^ ": "
      ^ (List.map (fun expr -> string_of_ast expr) exprs |> String.concat " ")
      ^ ")"
  | Sequence exprs ->
      "Sequence ("
      ^ (List.map (fun expr -> string_of_ast expr) exprs |> String.concat ";")
      ^ ")"
  | IntLit n -> "IntLit (" ^ string_of_int n ^ ")"
  | FloatLit f -> "FloatLit (" ^ string_of_float f ^ ")"
  | StringLit s -> "StringLit(" ^ s ^ ")"
  | BoolLit b -> "BoolLit (" ^ string_of_bool b ^ ")"
  | List l -> "List (" ^ (List.map string_of_ast l |> String.concat "; ") ^ ")"
  | If (e1, e2, e3) ->
      "If (" ^ string_of_ast e1 ^ ", " ^ string_of_ast e2 ^ ", "
      ^ string_of_ast e3 ^ ")"
  | Eq (e1, e2) -> "Eq (" ^ string_of_ast e1 ^ ", " ^ string_of_ast e2 ^ ")"
  | Less (e1, e2) -> "Less (" ^ string_of_ast e1 ^ ", " ^ string_of_ast e2 ^ ")"
  | Greater (e1, e2) ->
      "Greater (" ^ string_of_ast e1 ^ ", " ^ string_of_ast e2 ^ ")"
  | Plus (e1, e2) -> "Plus (" ^ string_of_ast e1 ^ ", " ^ string_of_ast e2 ^ ")"
  | Minus (e1, e2) ->
      "Minus (" ^ string_of_ast e1 ^ ", " ^ string_of_ast e2 ^ ")"
  | Times (e1, e2) ->
      "Times (" ^ string_of_ast e1 ^ ", " ^ string_of_ast e2 ^ ")"
  | Div (e1, e2) -> "Div (" ^ string_of_ast e1 ^ ", " ^ string_of_ast e2 ^ ")"
