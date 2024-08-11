open Ast

type tyvar = string

type ty =
  | TInt
  | TBool
  | TString
  | TArrow of ty * ty
  | TVar of tyvar
  | TList of ty

type tyenv = (string * ty) list
type tysubst = (tyvar * ty) list

let theta0 = ([] : tysubst)
let new_typevar n = (TVar ("'a" ^ string_of_int n), n + 1)

let rec lookup x env =
  match env with
  | [] -> failwith ("unbound variable: " ^ x)
  | (y, v) :: tl -> if x = y then v else lookup x tl

let rec occurs tx t =
  if tx = t then true
  else
    match t with TArrow (t1, t2) -> occurs tx t1 || occurs tx t2 | _ -> false

let rec subst_ty theta t =
  let rec subst_ty1 theta1 s =
    match theta1 with
    | [] -> TVar s
    | (tx, t1) :: theta2 -> if tx = s then t1 else subst_ty1 theta2 s
  in
  match t with
  | TInt -> TInt
  | TBool -> TBool
  | TString -> TString
  | TArrow (t2, t3) -> TArrow (subst_ty theta t2, subst_ty theta t3)
  | TVar s -> subst_ty1 theta s
  | TList t1 -> TList (subst_ty theta t1)

let subst_tyenv theta te = List.map (fun (x, t) -> (x, subst_ty theta t)) te

let subst_eql theta eql =
  List.map (fun (t1, t2) -> (subst_ty theta t1, subst_ty theta t2)) eql

let compose_subst theta2 theta1 =
  let theta11 = List.map (fun (tx, t) -> (tx, subst_ty theta2 t)) theta1 in
  List.fold_left
    (fun tau (tx, t) ->
      try
        let _ = lookup tx theta1 in
        tau
      with Failure _ -> (tx, t) :: tau)
    theta11 theta2

let unify eql =
  let rec solve eql theta =
    match eql with
    | [] -> theta
    | (t1, t2) :: eql2 -> (
        if t1 = t2 then solve eql2 theta
        else
          match (t1, t2) with
          | TArrow (t11, t12), TArrow (t21, t22) ->
              solve ((t11, t21) :: (t12, t22) :: eql2) theta
          | TList t1, TList t2 -> solve ((t1, t2) :: eql2) theta
          | TVar s, _ ->
              if occurs t1 t2 then failwith "unification failed"
              else
                solve
                  (subst_eql [ (s, t2) ] eql2)
                  (compose_subst [ (s, t2) ] theta)
          | _, TVar s ->
              if occurs t2 t1 then failwith "unification failed"
              else
                solve
                  (subst_eql [ (s, t1) ] eql2)
                  (compose_subst [ (s, t1) ] theta)
          | _, _ -> failwith "unification failed")
  in
  solve eql []

let tinf e =
  let aux te e n =
    match e with
    | IntLit _ -> (te, TInt, theta0, n)
    | _ -> failwith "not implemented"
  in
  aux [] e 0
