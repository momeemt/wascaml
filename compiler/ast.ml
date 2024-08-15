open Types

type ast =
  | Let of ty * string * string list * ast * ast
  | LetRec of ty * string * string list * ast * ast
  | App of ty * string * ast list
  | Fun of ty * string * ast
  | Sequence of ty * ast list
  | IntLit of ty * int
  | FloatLit of ty * float
  | StringLit of ty * string
  | BoolLit of ty * bool
  | List of ty * ast list
  | If of ty * ast * ast * ast
  | Eq of ty * ast * ast
  | Less of ty * ast * ast
  | Greater of ty * ast * ast
  | Plus of ty * ast * ast
  | Minus of ty * ast * ast
  | Times of ty * ast * ast
  | Div of ty * ast * ast
  | Cons of ty * ast * ast
  | Append of ty * ast * ast

let rec string_of_ast ast =
  match ast with
  | Let (ty, id, args, e1, e2) ->
      "Let (" ^ id ^ ", [" ^ String.concat "; " args ^ "], " ^ string_of_ast e1
      ^ ", " ^ string_of_ast e2 ^ ") : " ^ string_of_ty ty
  | LetRec (ty, f, args, e1, e2) ->
      "LetRec (" ^ f ^ ", [" ^ String.concat "; " args ^ "], "
      ^ string_of_ast e1 ^ ", " ^ string_of_ast e2 ^ ") : " ^ string_of_ty ty
  | App (ty, name, exprs) ->
      "App (" ^ name ^ ": "
      ^ (List.map (fun expr -> string_of_ast expr) exprs |> String.concat " ")
      ^ ") : " ^ string_of_ty ty
  | Fun (ty, arg, body) ->
      "Fun (" ^ arg ^ ", " ^ string_of_ast body ^ ") : " ^ string_of_ty ty
  | Sequence (ty, exprs) ->
      "Sequence ("
      ^ (List.map (fun expr -> string_of_ast expr) exprs |> String.concat ";")
      ^ ") : " ^ string_of_ty ty
  | IntLit (ty, n) -> "IntLit (" ^ string_of_int n ^ ") : " ^ string_of_ty ty
  | FloatLit (ty, f) ->
      "FloatLit (" ^ string_of_float f ^ ") : " ^ string_of_ty ty
  | StringLit (ty, s) -> "StringLit(" ^ s ^ ") : " ^ string_of_ty ty
  | BoolLit (ty, b) -> "BoolLit (" ^ string_of_bool b ^ ") : " ^ string_of_ty ty
  | List (ty, l) ->
      "List ("
      ^ (List.map string_of_ast l |> String.concat "; ")
      ^ ") : " ^ string_of_ty ty
  | If (ty, e1, e2, e3) ->
      "If (" ^ string_of_ast e1 ^ ", " ^ string_of_ast e2 ^ ", "
      ^ string_of_ast e3 ^ ") : " ^ string_of_ty ty
  | Eq (ty, e1, e2) ->
      "Eq (" ^ string_of_ast e1 ^ ", " ^ string_of_ast e2 ^ ") : "
      ^ string_of_ty ty
  | Less (ty, e1, e2) ->
      "Less (" ^ string_of_ast e1 ^ ", " ^ string_of_ast e2 ^ ") : "
      ^ string_of_ty ty
  | Greater (ty, e1, e2) ->
      "Greater (" ^ string_of_ast e1 ^ ", " ^ string_of_ast e2 ^ ") : "
      ^ string_of_ty ty
  | Plus (ty, e1, e2) ->
      "Plus (" ^ string_of_ast e1 ^ ", " ^ string_of_ast e2 ^ ") : "
      ^ string_of_ty ty
  | Minus (ty, e1, e2) ->
      "Minus (" ^ string_of_ast e1 ^ ", " ^ string_of_ast e2 ^ ") : "
      ^ string_of_ty ty
  | Times (ty, e1, e2) ->
      "Times (" ^ string_of_ast e1 ^ ", " ^ string_of_ast e2 ^ ") : "
      ^ string_of_ty ty
  | Div (ty, e1, e2) ->
      "Div (" ^ string_of_ast e1 ^ ", " ^ string_of_ast e2 ^ ") : "
      ^ string_of_ty ty
  | Cons (ty, e1, e2) ->
      "Cons (" ^ string_of_ast e1 ^ ", " ^ string_of_ast e2 ^ ") : "
      ^ string_of_ty ty
  | Append (ty, e1, e2) ->
      "Append (" ^ string_of_ast e1 ^ ", " ^ string_of_ast e2 ^ ") : "
      ^ string_of_ty ty
