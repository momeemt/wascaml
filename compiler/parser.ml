open Tokens
open Ast

exception ParseError of string

let rec parse_expr tokens = parse_let_expr tokens

and parse_let_expr tokens =
  match tokens with
  | Tokens.Let :: Recursive :: Identifier f :: tokens -> (
      let args, tokens = parse_args [] tokens in
      match tokens with
      | Equal :: tokens -> (
          let body, tokens = parse_expr tokens in
          match tokens with
          | In :: tokens ->
              let scope, tokens = parse_expr tokens in
              (LetRec (f, args, body, scope), tokens)
          | _ -> raise (ParseError "Expected 'in'"))
      | _ -> raise (ParseError "Expected '='"))
  | Let :: Identifier id :: tokens -> (
      let args, tokens = parse_args [] tokens in
      match tokens with
      | Equal :: tokens -> (
          let e1, tokens = parse_expr tokens in
          match tokens with
          | In :: tokens ->
              let e2, tokens = parse_expr tokens in
              (Let (id, args, e1, e2), tokens)
          | _ -> raise (ParseError "Expected 'in'"))
      | _ -> raise (ParseError "Expected '='"))
  | _ -> parse_fun_expr tokens

and parse_fun_expr tokens =
  match tokens with
  | Function :: Identifier id :: Arrow :: tokens ->
      let body, tokens = parse_expr tokens in
      (Fun (id, body), tokens)
  | _ -> parse_if_expr tokens

and parse_if_expr tokens =
  match tokens with
  | If :: tokens -> (
      let cond, tokens = parse_expr tokens in
      match tokens with
      | Then :: tokens -> (
          let then_expr, tokens = parse_expr tokens in
          match tokens with
          | Else :: tokens ->
              let else_expr, tokens = parse_expr tokens in
              (If (cond, then_expr, else_expr), tokens)
          | _ -> raise (ParseError "Expected 'else'"))
      | _ -> raise (ParseError "Expected 'then'"))
  | _ -> parse_equal_expr tokens

and parse_equal_expr tokens =
  let lhs, tokens = parse_add_sub_expr tokens in
  match tokens with
  | Equal :: tokens ->
      let rhs, tokens = parse_add_sub_expr tokens in
      (Eq (lhs, rhs), tokens)
  | Less :: tokens ->
      let rhs, tokens = parse_add_sub_expr tokens in
      (Less (lhs, rhs), tokens)
  | Greater :: tokens ->
      let rhs, tokens = parse_add_sub_expr tokens in
      (Greater (lhs, rhs), tokens)
  | _ -> (lhs, tokens)

and parse_add_sub_expr tokens =
  let rec aux acc tokens =
    match tokens with
    | Tokens.Plus :: tokens ->
        let rhs, tokens = parse_mul_div_expr tokens in
        aux (Plus (acc, rhs)) tokens
    | Hyphen :: tokens ->
        let rhs, tokens = parse_mul_div_expr tokens in
        aux (Minus (acc, rhs)) tokens
    | _ -> (acc, tokens)
  in
  let lhs, tokens = parse_mul_div_expr tokens in
  aux lhs tokens

and parse_mul_div_expr tokens =
  let rec aux acc tokens =
    match tokens with
    | Asterisk :: tokens ->
        let rhs, tokens = parse_primary_expr tokens in
        aux (Times (acc, rhs)) tokens
    | Slash :: tokens ->
        let rhs, tokens = parse_primary_expr tokens in
        aux (Div (acc, rhs)) tokens
    | _ -> (acc, tokens)
  in
  let lhs, tokens = parse_primary_expr tokens in
  aux lhs tokens

and parse_primary_expr tokens =
  match tokens with
  | Int n :: tokens -> (IntLit n, tokens)
  | Bool b :: tokens -> (BoolLit b, tokens)
  | Identifier id :: LeftParen :: tokens -> (
      let args, tokens = parse_expr_list tokens in
      match tokens with
      | RightParen :: tokens ->
          let rec build_app e args =
            match args with [] -> e | x :: xs -> build_app (App (e, x)) xs
          in
          (build_app (Identifier id) args, tokens)
      | _ -> raise (ParseError "Expected ')'"))
  | Identifier id :: tokens -> (Identifier id, tokens)
  | LeftParen :: tokens -> (
      let expr, tokens = parse_expr tokens in
      match tokens with
      | RightParen :: tokens -> (expr, tokens)
      | _ -> raise (ParseError "Expected ')'"))
  | rest -> raise (ParseError ("Expected primary expression: " ^ string_of_tokens rest))

and parse_expr_list tokens =
  let rec aux acc tokens =
    match tokens with
    | Comma :: tokens ->
        let expr, tokens = parse_expr tokens in
        aux (expr :: acc) tokens
    | _ -> (List.rev acc, tokens)
  in
  let expr, tokens = parse_expr tokens in
  aux [ expr ] tokens

and parse_args acc tokens =
  match tokens with
  | Identifier id :: tokens -> parse_args (id :: acc) tokens
  | _ -> (List.rev acc, tokens)

let parse tokens =
  Printf.printf "%s" (string_of_tokens tokens);
  let ast, tokens = parse_expr tokens in
  match tokens with
  | [ EOF ] -> ast
  | _ -> raise (ParseError "Unexpected tokens at the end")
