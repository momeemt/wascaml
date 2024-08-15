type tyvar = string

type ty =
  | TInvalid
  | TUnit
  | TInt
  | TBool
  | TString
  | TArrow of ty * ty
  | TVar of tyvar
  | TList of ty

type tyenv = (string * ty) list
type tysubst = (tyvar * ty) list

let string_of_ty t =
  let rec aux t =
    match t with
    | TInvalid -> "invalid"
    | TUnit -> "unit"
    | TInt -> "int"
    | TBool -> "bool"
    | TString -> "string"
    | TArrow (t1, t2) -> "(" ^ aux t1 ^ " -> " ^ aux t2 ^ ")"
    | TVar s -> s
    | TList t1 -> "(" ^ aux t1 ^ " list)"
  in
  aux t

let rec string_of_tyenv tenv =
  match tenv with
  | (str, t) :: rest ->
      Printf.sprintf "%s :: %s, " str (string_of_ty t) ^ string_of_tyenv rest
  | [] -> ""

let rec string_of_tysubst (tsubst : (tyvar * ty) list) =
  match tsubst with
  | (tyv, t) :: rest ->
      Printf.sprintf "%s :: %s, " tyv (string_of_ty t) ^ string_of_tysubst rest
  | [] -> ""
