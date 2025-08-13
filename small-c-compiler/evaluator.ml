open SmallCTypes
open Utils
open TokenTypes

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

(*we're evaluating t! with the environment*)
let rec eval_expr env t = match t with
  |Int(x) -> Int_Val(x)

  |Bool(x) -> Bool_Val(x)

  |ID(x) -> (match List.assoc_opt x env with
    | Some(v) -> v
    | None -> raise (DeclareError "no assign!"))

  |Add(v1, v2) -> let e1 = eval_expr env v1 in let e2 = eval_expr env v2 in
    (match e1, e2 with
      | Int_Val(e1), Int_Val(e2) ->  Int_Val(e1 + e2)
      | _, _ -> raise (TypeError "not ints!"))

  |Sub(v1, v2) -> let e1 = eval_expr env v1 in let e2 = eval_expr env v2 in
    (match e1, e2 with
      | Int_Val(v1), Int_Val(v2) -> Int_Val(v1 - v2)
      | _, _ -> raise (TypeError "not ints!"))    

  |Mult(v1, v2) -> let e1 = eval_expr env v1 in let e2 = eval_expr env v2 in
    (match e1, e2 with
      | Int_Val(v1), Int_Val(v2) -> Int_Val(v1 * v2)
      | _, _ -> raise (TypeError "not ints!"))  

  |Div(v1, v2) -> let e1 = eval_expr env v1 in let e2 = eval_expr env v2 in
    (match e1, e2 with
      | Int_Val(_), Int_Val(0) -> raise (DivByZeroError)
      | Int_Val(e1), Int_Val(e2) -> Int_Val(e1 / e2)
      | _, _ -> raise (TypeError "not ints!"))

  |Pow(v1, v2) -> let e1 = eval_expr env v1 in let e2 = eval_expr env v2 in
    (match e1, e2 with
      | Int_Val(e1), Int_Val(e2) -> Int_Val(int_of_float(float_of_int(e1) ** float_of_int(e2)))
      | _, _ -> raise (TypeError "not ints!"))  

  |Or(v1, v2) ->  let e1 = eval_expr env v1 in let e2 = eval_expr env v2 in
    (match e1, e2 with
      | Bool_Val(e1), Bool_Val(e2) -> Bool_Val(e1 || e2)
      | _, _ -> raise (TypeError "not bools!"))
  
  |And(v1, v2) -> let e1 = eval_expr env v1 in let e2 = eval_expr env v2 in
    (match e1, e2 with
      | Bool_Val(e1), Bool_Val(e2) -> Bool_Val(e1 && e2)
      | _, _ -> raise (TypeError "not bools!"))

  |Not(v) -> let e = eval_expr env v in 
    (match e with
      | Bool_Val(e) -> if e = true then Bool_Val(false) else Bool_Val(true)
      | _ -> raise (TypeError "not a bool!"))

  |Greater(v1, v2) -> let e1 = eval_expr env v1 in let e2 = eval_expr env v2 in
    (match e1, e2 with
      | Int_Val(e1), Int_Val(e2) -> Bool_Val(e1 > e2)
      | _, _ -> raise (TypeError "not ints!"))  

  |Less(v1, v2) -> let e1 = eval_expr env v1 in let e2 = eval_expr env v2 in
    (match e1, e2 with
      | Int_Val(e1), Int_Val(e2) -> Bool_Val(e1 < e2)
      | _, _ -> raise (TypeError "not ints!"))  

  |GreaterEqual(v1, v2) -> let e1 = eval_expr env v1 in let e2 = eval_expr env v2 in
    (match e1, e2 with
      | Int_Val(e1), Int_Val(e2) -> Bool_Val(e1 >= e2)
      | _, _ -> raise (TypeError "not ints!"))  

  |LessEqual(v1, v2) -> let e1 = eval_expr env v1 in let e2 = eval_expr env v2 in
    (match e1, e2 with
      | Int_Val(e1), Int_Val(e2) -> Bool_Val(e1 <= e2)
      | _, _ -> raise (TypeError "not ints!"))  

  |Equal(v1, v2) -> let e1 = eval_expr env v1 in let e2 = eval_expr env v2 in
    (match e1, e2 with
      | Int_Val(e1), Int_Val(e2) ->  Bool_Val(e1 = e2)
      | Bool_Val(e1), Bool_Val(e2) -> Bool_Val(e1 = e2)
      | _, _ -> raise (TypeError "not ints!"))

  |NotEqual(v1, v2) -> let e1 = eval_expr env v1 in let e2 = eval_expr env v2 in
    (match e1, e2 with
      | Int_Val(e1), Int_Val(e2) ->  Bool_Val(e1 <> e2)
      | Bool_Val(e1), Bool_Val(e2) -> Bool_Val(e1 <> e2)
      | _, _ -> raise (TypeError "not ints!"))

let rec eval_stmt env s = match s with
  |NoOp -> env

  |Seq(s1, s2) -> let env1 = eval_stmt env s1 in 
          let env2 = eval_stmt env1 s2 in env2

  |Declare(t, id) -> if not (List.mem_assoc id env) then
    (match t with
      |Int_Type -> (id, Int_Val(0)) :: env
      |Bool_Type -> (id, Bool_Val(false)) :: env
    ) else raise (DeclareError "error in declare")

  |Assign(x, e) -> if List.mem_assoc x env then let e1 = eval_expr env e in
    let t = List.assoc x env in (match (t, e1) with
      |Int_Val(_), Int_Val(_) -> (x, e1) :: List.remove_assoc x env
      |Bool_Val(_), Bool_Val(_) -> (x, e1) :: List.remove_assoc x env
      |_ -> raise (TypeError "error in assign"))
  else raise (DeclareError "error in assign")

  |If(guard, if_b, else_b) -> let e = eval_expr env guard in 
    (match e with
      |Bool_Val(g) -> if (g = true) then eval_stmt env if_b else eval_stmt env else_b
      |_ -> raise (TypeError "error in if"))

  |While(guard, body) -> let e = eval_expr env guard in 
    (match e with
      |Bool_Val(g) -> (match g with 
        |true -> let e1 = eval_stmt env body in eval_stmt e1 (While(guard, body))
        |false -> env)
      |_ -> raise (TypeError "error in if"))

  |For(id, e1, e2, body) -> let e1' = eval_expr env e1 in 
    let e2' = eval_expr env e2 in
    (match e1', e2' with
      |Int_Val(x1), Int_Val(x2) -> if (x1 <= x2) then
        let env' = (id, Int_Val(x1)) :: List.remove_assoc id env in
        let eval_body = eval_stmt env' body in 
        eval_stmt eval_body (For(id, Int(x1+1), e2, body))
      else env
      |_, _ -> raise (TypeError "error in if"))
  
  |Print(e) -> let e1 = eval_expr env e in (match e1 with
    |Int_Val(x) -> let _ = print_output_int x in let _ = print_output_newline() in env
    |Bool_Val(x) -> let _ = print_output_bool x in let _ = print_output_newline() in env)

