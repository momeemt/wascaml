open Tokens
open Ast
open Types

exception ParseError of string

let rec parse_expr tokens = parse_sequence_expr tokens

and parse_sequence_expr tokens =
  let rec aux acc tokens =
    let expr, tokens = parse_let_expr tokens in
    match tokens with
    | SemiColon :: tokens -> aux (expr :: acc) tokens
    | _ ->
        if acc = [] then (expr, tokens)
        else (Sequence (TInvalid, List.rev (expr :: acc)), tokens)
  in
  aux [] tokens

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
              (LetRec (TInvalid, f, args, body, scope), tokens)
          | _ -> raise (ParseError "Expected 'in'"))
      | rest -> raise (ParseError ("Expected '=': " ^ string_of_tokens rest)))
  | Let :: Identifier id :: tokens -> (
      let args, tokens = parse_args [] tokens in
      match tokens with
      | Equal :: tokens -> (
          let e1, tokens = parse_expr tokens in
          match tokens with
          | In :: tokens ->
              let e2, tokens = parse_expr tokens in
              (Let (TInvalid, id, args, e1, e2), tokens)
          | _ -> raise (ParseError "Expected 'in'"))
      | rest -> raise (ParseError ("Expected '=': " ^ string_of_tokens rest)))
  | _ -> parse_fun_expr tokens

and parse_fun_expr tokens =
  match tokens with
  | Function :: Identifier id :: Arrow :: tokens ->
      let body, tokens = parse_expr tokens in
      (Fun (TInvalid, id, body), tokens)
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
              (If (TInvalid, cond, then_expr, else_expr), tokens)
          | _ -> raise (ParseError "Expected 'else'"))
      | _ -> raise (ParseError "Expected 'then'"))
  | _ -> parse_equal_expr tokens

and parse_equal_expr tokens =
  let lhs, tokens = parse_add_sub_expr tokens in
  match tokens with
  | Equal :: tokens ->
      let rhs, tokens = parse_add_sub_expr tokens in
      (Eq (TInvalid, lhs, rhs), tokens)
  | Less :: tokens ->
      let rhs, tokens = parse_add_sub_expr tokens in
      (Less (TInvalid, lhs, rhs), tokens)
  | Greater :: tokens ->
      let rhs, tokens = parse_add_sub_expr tokens in
      (Greater (TInvalid, lhs, rhs), tokens)
  | _ -> (lhs, tokens)

and parse_add_sub_expr tokens =
  let rec aux acc tokens =
    match tokens with
    | Tokens.Plus :: tokens ->
        let rhs, tokens = parse_mul_div_expr tokens in
        aux (Plus (TInvalid, acc, rhs)) tokens
    | Hyphen :: tokens ->
        let rhs, tokens = parse_mul_div_expr tokens in
        aux (Minus (TInvalid, acc, rhs)) tokens
    | _ -> (acc, tokens)
  in
  let lhs, tokens = parse_mul_div_expr tokens in
  aux lhs tokens

and parse_mul_div_expr tokens =
  let rec aux acc tokens =
    match tokens with
    | Asterisk :: tokens ->
        let rhs, tokens = parse_primary_expr tokens in
        aux (Times (TInvalid, acc, rhs)) tokens
    | Slash :: tokens ->
        let rhs, tokens = parse_primary_expr tokens in
        aux (Div (TInvalid, acc, rhs)) tokens
    | _ -> (acc, tokens)
  in
  let lhs, tokens = parse_list_ops_expr tokens in
  aux lhs tokens

and parse_list_ops_expr tokens =
  let rec aux acc tokens =
    match tokens with
    | AtSign :: tokens ->
        let rhs, tokens = parse_expr tokens in
        aux (Append (TInvalid, acc, rhs)) tokens
    | DoubleColon :: tokens ->
        let rhs, tokens = parse_expr tokens in
        aux (Cons (TInvalid, acc, rhs)) tokens
    | _ -> (acc, tokens)
  in
  let lhs, tokens = parse_primary_expr tokens in
  aux lhs tokens

and parse_primary_expr tokens =
  match tokens with
  | Int n :: tokens -> (IntLit (TInvalid, n), tokens)
  | Bool b :: tokens -> (BoolLit (TInvalid, b), tokens)
  | String s :: tokens -> (StringLit (TInvalid, s), tokens)
  | Identifier id :: tokens ->
      let args, tokens = parse_app tokens in
      (App (TInvalid, id, args), tokens)
  | LeftParen :: tokens -> (
      let expr, tokens = parse_expr tokens in
      match tokens with
      | RightParen :: tokens -> (expr, tokens)
      | _ -> raise (ParseError "Expected ')'"))
  | LeftBracket :: tokens ->
      let exprs, tokens = parse_list tokens in
      (List (TInvalid, exprs), tokens)
  | _ -> raise (ParseError "Expected primary expression")

and parse_list tokens =
  let rec aux acc tokens =
    match List.hd tokens with
    | RightBracket -> (List.rev acc, List.tl tokens)
    | _ ->
        let expr, tokens = parse_expr tokens in
        aux (expr :: acc) tokens
  in
  aux [] tokens

and parse_app tokens : ast list * token list =
  let rec aux acc tokens =
    match List.hd tokens with
    | LeftParen | LeftBracket | Int _ | Float _ | String _ | Bool _
    | Identifier _ ->
        let ast, tokens = parse_primary_expr tokens in
        aux (ast :: acc) tokens
    | _ -> (List.rev acc, tokens)
  in
  aux [] tokens

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
  let ast, tokens = parse_expr tokens in
  match tokens with
  | [ EOF ] -> ast
  | rest ->
      raise
        (ParseError ("Unexpected tokens at the end: " ^ string_of_tokens rest))
