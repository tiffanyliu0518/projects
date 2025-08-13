open Ast
open Utils

let rec optimize e = 
  let rec lookup env x = match env with
  | [] -> raise (DeclareError ("not in env!"))
  | (str, v)::xs -> if str = x then v else lookup xs x in

    let rec fold oper e1 e2 = match oper, e1, e2 with
    (*----------------*)
        (*case adding int to int*)
      | Add, Int(0), x -> x
      | Add, x, Int(0) -> x
      | Add, Int(e1), Int(e2) -> Int(e1 + e2)

      | Add, Int(e1), Binop(Add,Int(e2),e) -> Binop (Add, Int(e1 + e2), e) 
      | Add, Binop(Add,Int(e1),e), Int(e2) -> Binop (Add, Int(e1 + e2), e)

      | Add,Binop(Add,Int(e1),e), Value -> Binop(Add, Int(e1), Binop(Add, Value, e))
      | Add, Value, Binop(Add,Int(e1),e) -> Binop(Add, Int(e1), Binop(Add, Value, e))

      | Add, Value, Int(e1) -> Binop(Add, Int(e1), Value)
      | Add, Binop(Add,Value,e1), e2 -> Binop (Add, e2, Binop(Add,Value,e1))
    (*---------------------*)

        | Mult, Int(0), x -> Int(0)
        | Mult, x, Int(0) -> Int(0)

        | Sub, x, Int(0) -> x

        | And, x, Bool(true) -> x
        | And, Bool(true), x -> x
        | And, x, Bool(false) -> Bool(false)
        | And, Bool(false), x -> Bool(false)

        | Or, x, Bool(true) -> Bool(true)
        | Or, Bool(true), x -> Bool(true)
        | Or, x, Bool(false) -> x
        | Or, Bool(false), x -> x
        
        | Sub, Int(v1), Int(0) -> Int(v1)
        | Sub, Int(v1), Int(v2) -> Int(v1 - v2)

        | Mult, Int(0), Int(0)  -> Int(0)
        | Mult, Int(0), Int(v2) -> Int(0)
        | Mult, Int(v1), Int(0) -> Int(0)
        | Mult, Int(v1), Int(v2) -> Int(v1 * v2)

        | Div, Int(v1), Int(0) -> raise DivByZeroError
        | Div, Value, Int(0) -> raise DivByZeroError
        | Div, x, Int(0) -> raise DivByZeroError
        | Div, Int(v1), Int(v2) -> Int(v1 / v2)

        | Pow, Int(v1), Int(v2) -> Int (int_of_float(float_of_int v1 ** float_of_int v2))

        | Greater, Int(v1), Int(v2) -> Bool (v1 > v2)
        | Less, Int(v1), Int(v2) -> Bool (v1 < v2)
        | GreaterEqual, Int(v1), Int(v2) -> Bool (v1 >= v2)
        | LessEqual, Int(v1), Int(v2) -> Bool (v1 <= v2)

        | Greater, Bool(v1), Bool(v2) -> Bool (v1 > v2)
        | Less, Bool(v1), Bool(v2) -> Bool (v1 < v2)
        | GreaterEqual, Bool(v1), Bool(v2) -> Bool (v1 >= v2)
        | LessEqual, Bool(v1), Bool(v2) -> Bool (v1 <= v2)

        | Equal, Int(v1), Int(v2) -> Bool(v1 = v2)
        | NotEqual, Int(v1), Int(v2) -> Bool(v1 <> v2)

        | Equal, Bool(v1), Bool(v2) -> Bool(v1 = v2)
        | NotEqual, Bool(v1), Bool(v2) -> Bool(v1 <> v2)

        | Or, Bool(v1), Bool(v2) -> Bool(v1 || v2)
        | And, Bool(v1), Bool(v2) -> Bool(v1 && v2)

        |_ -> Binop(oper, e1, e2) in

    let rec const_prop_stmt env stmt = match stmt with
        |NoOp -> env, NoOp
        (*recursively go all the way down to get the end expression, updated env*)
        | Assign (str, t, expr) -> let new_expr, new_env = const_prop_expr env expr in
            (match new_expr with
                (*takes the updated constant and adds to the env*)
                | Int(_) -> let new_new_env = (str, new_expr) :: (List.remove_assoc str env) in
                    (new_new_env, Assign(str, t, new_expr))
                | Bool(_) -> let new_new_env = (str, new_expr) :: (List.remove_assoc str env) in
                    (new_new_env, Assign(str, t, new_expr))
                | Value -> let new_new_env = (str, new_expr) :: (List.remove_assoc str env) in
                    (new_new_env, Assign(str, t, new_expr))
                (*removes var from the environment*)
                | _ -> let new_new_env = List.remove_assoc str env in
                    (new_new_env, Assign(str, t, new_expr)))

        | If (guard, if_b, else_b) -> let new_guard, new_env = const_prop_expr env guard in 
            let _, new_if = const_prop_stmt env if_b in
            let _, new_else = const_prop_stmt env else_b in
            (match new_guard with
                | Bool(true) -> (env, new_if)
                | Bool(false) -> (env, new_else)
                | Int(_) -> raise (TypeError "guard is an int??!")
                | _ -> 
                  if new_if = new_else then 
                      (env, new_if)
                  else
                      (env, If(new_guard, new_if, new_else)))

        | While (guard, body) -> let new_guard, _ = const_prop_expr env guard in 
            (match new_guard with
                | Bool(false) -> (env, NoOp)
                | Bool(true) -> let _, new_body = const_prop_stmt env body in
                    (env, While(new_guard, new_body))
                |_ -> raise (TypeError "guard not a bool!"))

        | For (str, start, ending, body) -> let new_start, _ = const_prop_expr env start in
            let new_end, _ = const_prop_expr env ending in 
                (match new_start, new_end with
                    | Int(s), Int(e) -> if s > e then
                        let new_env = (str, new_start) :: env in
                        (new_env, Assign(str, Int_Type, new_start))

                    else if s = e then 
                        let new_env = (str, new_start) :: env in 
                        let _, new_body = const_prop_stmt new_env body in 
                            (env, Seq(Assign(str, Int_Type, new_start), new_body))

                    else let new_env = (str, new_start) :: env in 
                        let _, new_body = const_prop_stmt new_env body in 
                            (env, For(str, new_start, new_end, new_body))

                    | _ -> let new_env = (str, new_start) :: env in 
                        let _, new_body = const_prop_stmt new_env body in 
                            (env, For(str, new_start, new_end, new_body)))

        | Seq (e1, e2) -> let env1, new_e1 = const_prop_stmt env e1 in 
            let env2, new_e2 = const_prop_stmt env1 e2 in 
                (env2, Seq(new_e1, new_e2))

        | Print e -> let new_e, new_env = const_prop_expr env e in
            (new_env, Print(new_e))

    and const_prop_expr env e = match e with
        | Int(_) -> e, env
        | Bool(_) -> e, env
        | Value -> e, env
        | ID(str) -> (match List.assoc_opt str env with
          | Some(Int _ | Bool _ as v) -> v, env 
          | Some(_) -> ID(str), env  
          | None -> raise (DeclareError ("Undeclared identifier: " ^ str)))

        | Binop(operation, e1, e2) -> 
            let new_e1, env1 = const_prop_expr env e1 in
            let new_e2, env2 = const_prop_expr env1 e2 in 
                (fold operation new_e1 new_e2, env2)

        | Not(e1) -> let new_e, new_env = const_prop_expr env e1 in
            let rec fold_not e = (match e with
            | Not(x) -> if fold_not x = x then Not(x) else x
            | Bool(x) -> Bool(not x)
            | _ -> Not(e))
        in fold_not new_e, new_env
    
    in let _, opt_ast = const_prop_stmt [] e in opt_ast


let rec typecheck e = 
  let rec lookup env x = match env with
    | [] -> None
    | (str, v)::xs -> if str = x then Some(v) else lookup xs x in
    
  let rec check_type x1 x2 = match (x1, x2) with
    |(_, Unknown_Type _) -> true
    |(Unknown_Type _, _) -> true
    |(a, b) -> a = b in 
    
      (* Modified to return (type, env) *)
  let rec check_e env e = match e with
    | Int(_) -> (Int_Type, env)
    | ID (x) -> (match lookup env x with 
      | Some(v) -> (v, env) 
      | None -> raise (DeclareError "not in env!"))
    | Bool(_) -> (Bool_Type, env)
    | Value -> (Unknown_Type 0, env)
    
    | Binop(oper, e1, e2) -> 
      let (t1, env1) = check_e env e1 in
      let (t2, env2) = check_e env e2 in
        (match oper with
          | Add | Sub | Div | Pow | Mult ->
            if check_type t1 Int_Type && check_type t2 Int_Type then (Int_Type, env)
              else raise (TypeError "arithmetic op needs ints")
    
          | Equal | NotEqual -> 
            if check_type t1 t2 then (Bool_Type, env)
              else raise (TypeError "equality needs same types")
    
          | Greater | GreaterEqual | Less | LessEqual -> 
            if check_type t1 Int_Type && check_type t2 Int_Type then (Bool_Type, env)
              else if check_type t1 Bool_Type && check_type t2 Bool_Type then (Bool_Type, env)
              else raise (TypeError "comparison needs ints or bools")
    
          | And | Or -> 
            if check_type t1 Bool_Type && check_type t2 Bool_Type 
              then (Bool_Type, env) 
              else raise (TypeError "logical op needs bools"))
    
    | Not x -> 
      let (t, env') = check_e env x in
        if check_type t Bool_Type then (Bool_Type, env)
        else raise (TypeError "not needs bool") in 
    
  let rec check_helper env stmt = match stmt with
    | NoOp -> (Bool_Type, env) 
        
    | Assign(x, t, e) -> 
      let (e_type, env') = check_e env e in
        if not (check_type t e_type) then raise (TypeError "assignment type mismatch")
          else (Bool_Type, (x, t) :: env)
          
    | Seq(s1, s2) -> 
      let (_, env1) = check_helper env s1 in check_helper env1 s2
    
    | If (guard, if_b, else_b) -> let (guard_t, new_env) = check_e env guard in
      if not (check_type guard_t Bool_Type) then raise (TypeError "guard must be bool")
        else let (if_t, env_if) = check_helper env if_b in
          let (else_t, env_else) = check_helper env else_b in
            if not (check_type if_t else_t) then raise (TypeError "branches must have same type")
            else (if_t, new_env) 
    
    | While (guard, body) -> let (guard_t, new_env) = check_e env guard in
      if not (check_type guard_t Bool_Type) then raise (TypeError "while guard must be bool")
        else let (_, new_new_env) = check_helper new_env body in (Bool_Type, new_new_env)
    
    | For (str, start, ending, body) -> (match lookup env str with
      | Some(x) when (not (check_type x Int_Type)) -> raise (TypeError "loop var must be int")
      | _ -> let (start_t, _) = check_e env start in
        let (end_t, _) = check_e env ending in
          if not (check_type start_t Int_Type && check_type end_t Int_Type) then raise (TypeError "start/end must be ints")
          else let past = List.assoc_opt str env in  let new_env = (str, Int_Type)::env in 
            let (body_t, body_env) = check_helper new_env body in
            let final_env = (match past with
              | Some(x) -> (str, x) :: body_env
              | None -> List.remove_assoc str body_env) in
                  (Bool_Type, final_env))
    
    | Print x -> let (t, env') = check_e env x in (Bool_Type, env) in
    
  let (_, final_env) = check_helper [] e in true

let rec infer e =
  let rec lookup env x = match env with
    | [] -> None
    | (str, v)::xs -> if str = x then Some(v) else lookup xs x in
  
    let rec infer_e env e = 
      match e with
      | Int(_) -> (Int_Type, [])
      | Bool(_) -> (Bool_Type, [])
      | Value -> (Unknown_Type(fresh()), [])
      | ID(x) -> (match lookup env x with 
          | Some(v) -> (v, []) 
          | None -> raise (DeclareError "not in env!"))
            
      | Binop(op, e1, e2) ->
          let t1, c1 = infer_e env e1 in
          let t2, c2 = infer_e env e2 in
          (match op with
          | Add | Sub | Mult | Div | Pow ->
              (Int_Type, (t1, Int_Type) :: (t2, Int_Type) :: (c1 @ c2))
          | And | Or ->
              (Bool_Type, (t1, Bool_Type) :: (t2, Bool_Type) :: (c1 @ c2))
          | Equal | NotEqual ->
              (Bool_Type, (t1, t2) :: (c1 @ c2))
          | Greater | Less | GreaterEqual | LessEqual ->
              (Bool_Type, (t1, Int_Type) :: (t2, Int_Type) :: (c1 @ c2)))
      
      | Not(e) ->
          let t, c = infer_e env e in
          (Bool_Type, (t, Bool_Type) :: c)
    in
  
    
    let rec infer_s env stmt = 
      match stmt with
      | NoOp -> ([], env)
      
      | Assign(x, t, e) ->
        let te, ce = infer_e env e in
        ((t, te) :: ce, (x, t) :: env )
      
      | Seq(s1, s2) ->
          let c1, env1 = infer_s env s1 in
          let c2, env2 = infer_s env1 s2 in
          (c1 @ c2, env2)
      
    | If(guard, s1, s2) ->
        let tg, cg = infer_e env guard in
          (match tg with 
            | Int_Type -> raise (TypeError "help")
            | _ -> 
        let c1, t_1 = infer_s env s1 in
        let c2, t_2 = infer_s env s2 in
          ((tg, Bool_Type) :: cg @ c1 @ c2, env))
      
    | While(guard, body) ->
        let tg, cg = infer_e env guard in
        let cb, body_env = infer_s env body in
        let temp_con = cb in
        let loop_con = 
          List.fold_left (fun acc (x, t) ->
            match lookup env x with
            | Some(env_t) -> (env_t, t) :: acc
            | None -> acc
          ) [] body_env
        in
        ((tg, Bool_Type) :: cg @ temp_con @ loop_con, env)


    | For(x, e1, e2, body) ->
        let t1, c1 = infer_e env e1 in
        let t2, c2 = infer_e env e2 in
        let x_in_env = lookup env x in
    
        (match x_in_env with
        | Some(v) -> 
            let env_with_int = (x, Int_Type) :: env in
            let cb, body_env = infer_s env_with_int body in
            let final_env = (x, v) :: List.remove_assoc x body_env in
            ((t1, Int_Type) :: (t2, Int_Type) :: c1 @ c2 @ cb, final_env)
        
        | None -> 
            let env_with_int = (x, Int_Type) :: env in
            let cb, body_env = infer_s env_with_int body in
            let final_env = List.remove_assoc x body_env in
            ((t1, Int_Type) :: (t2, Int_Type) :: c1 @ c2 @ cb, final_env))
      
    | Print(e) ->
      let _, c = infer_e env e in (c, env) in

    let rec subs sub t =
      match t with
        | Unknown_Type n -> 
          (match List.assoc_opt (Unknown_Type n) sub with
            | Some concrete_type -> concrete_type
            | None -> t)
        | _ -> t in 
    
    let rec sub_s sub stmt =
      match stmt with
      | NoOp -> NoOp
      | Assign(x, t, e) -> 
          let new_t = subs sub t in
          let new_e = sub_e sub e in
            (match new_t with
             | Unknown_Type _ -> 
                (match new_e with
                  | Int _ -> Assign(x, Int_Type, new_e)

                  | Bool _ -> Assign(x, Bool_Type, new_e)

                  | _ -> Assign(x, new_t, new_e))

             | _ -> Assign(x, new_t, new_e))

        | Seq(s1, s2) -> Seq(sub_s sub s1, sub_s sub s2)

        | If(guard, s1, s2) -> If(sub_e sub guard, sub_s sub s1, sub_s sub s2)

        | While(guard, body) -> 
            While(sub_e sub guard, sub_s sub body)

        | For(x, e1, e2, body) -> 
            For(x, sub_e sub e1, sub_e sub e2, sub_s sub body)
                
        | Print(e) -> Print(sub_e sub e)
    
    and sub_e sub e =
      match e with
        | Int(_) | Bool(_) | Value -> e
        
        | ID(x) -> ID(x)

        | Binop(op, e1, e2) -> Binop(op, sub_e sub e1, sub_e sub e2)
                  
        | Not(e) -> 
            Not(sub_e sub e) in 
    
    let rec unify cons sub =
      match cons with
        | [] -> sub

        | (t1, t2)::rest when t1 = t2 -> 
            unify rest sub

        | (Int_Type, Bool_Type)::_ | (Bool_Type, Int_Type)::_ ->
            raise (TypeError "Cannot unify int and bool")

        | ((Unknown_Type n as var), t)::rest | (t, (Unknown_Type n as var))::rest ->
            let new_rest = List.map (fun (a, b) -> (subs [(var, t)] a, subs [(var, t)] b)) rest in

            let new_subs = List.map (fun (a, b) -> (subs [(var, t)] a, subs [(var, t)] b)) sub in
              unify new_rest ((var, t) :: new_subs)

        | _ -> raise (TypeError "Incompatible types") in
    
      let cons, _ = infer_s [] e in
      let sub = unify cons [] in
      sub_s sub e
