open Ast

type tyvar = string

type ty =
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

let theta0 = ([] : tysubst)
let new_typevar n = (TVar ("'a" ^ string_of_int n), n + 1)

let rec lookup x env =
  match env with
  | [] -> failwith ("unbound variable: " ^ x)
  | (y, v) :: tl -> if x = y then v else lookup x tl

let rec occurs tx t =
  if tx = t then true
  else
    match t with
    | TArrow (t1, t2) -> occurs tx t1 || occurs tx t2
    | TList t1 -> occurs tx t1
    | _ -> false

let rec subst_ty theta t =
  let rec subst_ty1 theta1 s =
    match theta1 with
    | [] -> TVar s
    | (tx, t1) :: theta2 -> if tx = s then t1 else subst_ty1 theta2 s
  in
  match t with
  | TUnit -> TUnit
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
              if occurs t1 t2 then failwith ("unification failed: " ^ s)
              else
                solve
                  (subst_eql [ (s, t2) ] eql2)
                  (compose_subst [ (s, t2) ] theta)
          | _, TVar s ->
              if occurs t2 t1 then failwith ("unification failed: " ^ s)
              else
                solve
                  (subst_eql [ (s, t1) ] eql2)
                  (compose_subst [ (s, t1) ] theta)
          | t1, t2 ->
              failwith
                ("unification failed: " ^ string_of_ty t1 ^ " = "
               ^ string_of_ty t2))
  in
  solve eql []

let tinf e =
  let rec aux te e n =
    match e with
    | IntLit _ -> (te, TInt, theta0, n)
    | StringLit _ -> (te, TString, theta0, n)
    | List [] ->
        let t, new_n = new_typevar n in
        (te, TList t, theta0, new_n)
    | List (h :: tl) ->
        let te1, t1, theta1, n1 = aux te h n in
        let te2, types, theta2, n2 =
          List.fold_left
            (fun (te_acc, t_acc, theta_acc, n_acc) e ->
              let te_next, t_next, theta_next, n_next = aux te_acc e n_acc in
              let t_next' = subst_ty theta_next t_next in
              let theta_acc' = compose_subst theta_next theta_acc in
              ( subst_tyenv theta_acc' te_next,
                t_acc @ [ t_next' ],
                theta_acc',
                n_next ))
            (te1, [], theta0, n1) tl
        in
        let _ = List.iter (fun t -> ignore (unify [ (t1, t) ])) types in
        let t11 = subst_ty theta2 t1 in
        let theta3 = unify [ (t11, t1) ] in
        let te3 = subst_tyenv theta3 te2 in
        let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
        (te3, TList t11, theta4, n2)
    | Cons (h, tl) ->
        let te1, t1, theta1, n1 = aux te h n in
        let te2, t2, theta2, n2 = aux te1 tl n1 in
        let t11 = subst_ty theta2 t1 in
        let theta3 = unify [ (t2, TList t11) ] in
        let te3 = subst_tyenv theta3 te2 in
        let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
        (te3, TList t11, theta4, n2)
    | Append (l1, l2) ->
        let te1, t1, theta1, n1 = aux te l1 n in
        let te2, t2, theta2, n2 = aux te1 l2 n1 in
        let t11 = subst_ty theta2 t1 in
        let theta3 = unify [ (t2, t11) ] in
        let te3 = subst_tyenv theta3 te2 in
        let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
        (te3, t2, theta4, n2)
    | Plus (e1, e2) | Minus (e1, e2) | Times (e1, e2) | Div (e1, e2) ->
        let te1, t1, theta1, n1 = aux te e1 n in
        let te2, t2, theta2, n2 = aux te1 e2 n1 in
        let t11 = subst_ty theta2 t1 in
        let theta3 = unify [ (t11, TInt); (t2, TInt) ] in
        let te3 = subst_tyenv theta3 te2 in
        let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
        (te3, TInt, theta4, n2)
    | Eq (e1, e2) | Less (e1, e2) | Greater (e1, e2) ->
        let te1, t1, theta1, n1 = aux te e1 n in
        let te2, t2, theta2, n2 = aux te1 e2 n1 in
        let t11 = subst_ty theta2 t1 in
        let theta3 = unify [ (t11, TInt); (t2, TInt) ] in
        let te3 = subst_tyenv theta3 te2 in
        let theta4 = compose_subst theta3 (compose_subst theta2 theta1) in
        (te3, TBool, theta4, n2)
    | If (e1, e2, e3) ->
        let te1, t1, theta1, n1 = aux te e1 n in
        let te2, t2, theta2, n2 = aux te1 e2 n1 in
        let te3, t3, theta3, n3 = aux te2 e3 n2 in
        let t11 = subst_ty theta3 t1 in
        let theta4 = unify [ (t11, TBool); (t2, t3) ] in
        let te4 = subst_tyenv theta4 te3 in
        let theta5 =
          compose_subst theta4
            (compose_subst theta3 (compose_subst theta2 theta1))
        in
        (te4, t2, theta5, n3)
    | Let (name, params, value, body) ->
        let param_types, n1 =
          List.fold_right
            (fun _ (types, n) ->
              let t, n' = new_typevar n in
              (t :: types, n'))
            params ([], n)
        in
        let te1 =
          List.fold_left2
            (fun te param t -> (param, t) :: te)
            te params param_types
        in
        let te2, t_value, theta1, n2 = aux te1 value n1 in
        let t_func =
          List.fold_right
            (fun t_arg t_acc -> TArrow (t_arg, t_acc))
            param_types t_value
        in
        let te3 = (name, t_func) :: subst_tyenv theta1 te2 in
        let te4, t_body, theta2, n3 = aux te3 body n2 in
        let theta3 = compose_subst theta2 theta1 in
        (te4, t_body, theta3, n3)
    | LetRec (name, params, value, body) ->
        let param_types, n1 =
          List.fold_right
            (fun _ (types, n) ->
              let t, n' = new_typevar n in
              (t :: types, n'))
            params ([], n)
        in
        let t_ret, n2 = new_typevar n1 in
        let t_func =
          List.fold_right
            (fun t_arg t_acc -> TArrow (t_arg, t_acc))
            param_types t_ret
        in
        let te1 = (name, t_func) :: te in
        let te2 =
          List.fold_left2
            (fun te param t -> (param, t) :: te)
            te1 params param_types
        in
        let te3, t_value, theta1, n3 = aux te2 value n2 in
        let theta_func = unify [ (t_ret, t_value) ] in
        let te4 = subst_tyenv theta_func te3 in
        let theta2 = compose_subst theta_func theta1 in
        let te5, t_body, theta3, n4 = aux te4 body n3 in
        let theta4 = compose_subst theta3 theta2 in
        (te5, t_body, theta4, n4)
    | App (func_name, args) ->
        let t_func =
          try List.assoc func_name te
          with Not_found -> failwith ("Unknown function: " ^ func_name)
        in
        let te', arg_types, theta', n' =
          List.fold_left
            (fun (te_acc, types_acc, theta_acc, n_acc) arg ->
              let te_next, t_arg, theta_next, n_next = aux te_acc arg n_acc in
              let t_arg' = subst_ty theta_acc t_arg in
              let theta_acc' = compose_subst theta_next theta_acc in
              ( subst_tyenv theta_acc' te_next,
                types_acc @ [ t_arg' ],
                theta_acc',
                n_next ))
            (te, [], theta0, n) args
        in
        let t_ret, n2 = new_typevar n' in
        let t_func_expected =
          List.fold_right
            (fun t_arg t_acc -> TArrow (t_arg, t_acc))
            arg_types t_ret
        in
        let theta_func = unify [ (t_func, t_func_expected) ] in
        let te_final = subst_tyenv theta_func te' in
        let theta_final = compose_subst theta_func theta' in
        (te_final, subst_ty theta_final t_ret, theta_final, n2)
    | Sequence exprs ->
        let rec aux_sequence te exprs n =
          match exprs with
          | [] -> (te, TUnit, theta0, n)
          | [ e ] -> aux te e n
          | e :: rest ->
              let te1, t1, theta1, n1 = aux te e n in
              let te2 = unify [ (t1, TUnit) ] in
              let te3 = subst_tyenv te2 te1 in
              let te4, t2, theta2, n2 = aux_sequence te3 rest n1 in
              (te4, t2, compose_subst theta2 theta1, n2)
        in
        aux_sequence te exprs n
    | rest -> failwith ("not implemented: " ^ string_of_ast rest)
  in
  let t1, t2, t3, t4 =
    aux
      [
        ("print_int32", TArrow (TInt, TUnit));
        ("print_string", TArrow (TString, TUnit));
        ("print_list", TArrow (TList TInt, TUnit));
        ("list_length", TArrow (TList TInt, TInt));
        ("discard", TArrow (TVar "'discard", TUnit));
        ("list_hd", TArrow (TList TInt, TInt));
        ("list_next", TArrow (TList TInt, TList TInt));
        ("list_copy", TArrow (TList TInt, TArrow (TInt, TList TInt)));
        ("list_copy_continue", TArrow (TList TInt, TArrow (TInt, TList TInt)));
      ]
      e 1
  in
  (t1, t2, t3, t4)
