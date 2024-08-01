type ast =
  | IntLit of int
  | Plus of ast * ast
  | Minus of ast * ast
  | Times of ast * ast
  | Div of ast * ast

let rec string_of_ast ast =
  match ast with
  | IntLit n -> "IntLit (" ^ string_of_int n ^ ")"
  | Plus (left, right) ->
      "Plus (" ^ string_of_ast left ^ ", " ^ string_of_ast right ^ ")"
  | Minus (left, right) ->
      "Minus (" ^ string_of_ast left ^ ", " ^ string_of_ast right ^ ")"
  | Times (left, right) ->
      "Times (" ^ string_of_ast left ^ ", " ^ string_of_ast right ^ ")"
  | Div (left, right) ->
      "Div (" ^ string_of_ast left ^ ", " ^ string_of_ast right ^ ")"
