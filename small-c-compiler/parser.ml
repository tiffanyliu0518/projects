open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)

let rec parse_expr toks : expr_result = 
  parse_or toks

  (*Or -> Or||Or | And *)
  and parse_or toks = let t1, e1 = parse_and toks in
   (*looks to see if the next token is also Or*)
    match lookahead t1 with
      |Tok_Or -> (*Or -> Or||Or*)
        (*gets the contents of the second or*)
        let t2 = match_token t1 Tok_Or in 
        let t3, e2 = parse_or t2 in
          (t3, Or(e1, e2))
      |_ -> (t1, e1) (*Or -> And*)

  (*And -> And && And | Equality*)
  and parse_and toks = let t1, e1 = parse_equality toks in
    match lookahead t1 with
      |Tok_And -> (*And -> And && And*)
        let t2 = match_token t1 Tok_And in 
        let t3, e2 = parse_and t2 in
          (t3, And(e1, e2))
      |_ -> (t1, e1) (*And -> Equality*)

  and parse_equality toks = let t1, e1 = parse_relation toks in 
    match lookahead t1 with
      |Tok_Equal -> 
        let t2 = match_token t1 Tok_Equal in
        let t3, e2 = parse_equality t2 in
          (t3, Equal(e1, e2))
      |Tok_NotEqual -> 
        let t2 = match_token t1 Tok_NotEqual in
        let t3, e2 = parse_equality t2 in
          (t3, NotEqual(e1, e2))
      |_ -> (t1, e1)

  and parse_relation toks = let t1, e1 = parse_add toks in
    match lookahead t1 with
      |Tok_Less -> 
        let t2 = match_token t1 Tok_Less in
        let t3, e2 = parse_relation t2 in
          (t3, Less(e1, e2))

      |Tok_Greater -> 
        let t2 = match_token t1 Tok_Greater in
        let t3, e2 = parse_relation t2 in
          (t3, Greater(e1, e2))

      |Tok_LessEqual -> 
        let t2 = match_token t1 Tok_LessEqual in
        let t3, e2 = parse_relation t2 in
          (t3, LessEqual(e1, e2))

      |Tok_GreaterEqual -> 
        let t2 = match_token t1 Tok_GreaterEqual in
        let t3, e2 = parse_relation t2 in
          (t3, GreaterEqual(e1, e2))  

      |_ -> (t1, e1)  

  and parse_add toks = let t1, e1 = parse_mult toks in
    match lookahead t1 with
      |Tok_Add -> 
        let t2 = match_token t1 Tok_Add in
        let t3, e2 = parse_add t2 in
          (t3, Add(e1, e2)) 

      |Tok_Sub -> 
        let t2 = match_token t1 Tok_Sub in
        let t3, e2 = parse_add t2 in
          (t3, Sub(e1, e2)) 
        
      |_ -> (t1, e1)
  
  and parse_mult toks = let t1, e1 = parse_power toks in 
    match lookahead t1 with
      |Tok_Mult -> 
        let t2 = match_token t1 Tok_Mult in
        let t3, e2 = parse_mult t2 in
          (t3, Mult(e1, e2)) 

      |Tok_Div -> 
        let t2 = match_token t1 Tok_Div in
        let t3, e2 = parse_mult t2 in
          (t3, Div(e1, e2)) 
      
      |_ -> (t1, e1)
    
  and parse_power toks = let t1, e1 = parse_unary toks in
    match lookahead t1 with
      |Tok_Pow -> 
        let t2 = match_token t1 Tok_Pow in
        let t3, e2 = parse_power t2 in
          (t3, Pow(e1, e2))

      |_ -> (t1, e1)

  and parse_unary toks = 
    match lookahead toks with
      |Tok_Not -> 
        let t1 = match_token toks Tok_Not in
        let t2, e2 = parse_unary t1 in
          (t2, Not(e2))
      
      |_ -> parse_pri toks

  and parse_pri toks = match toks with
    |Tok_Int(n)::t -> let t = match_token toks (Tok_Int(n)) in
      (t, Int(n))

    |Tok_Bool(n)::t -> let t = match_token toks (Tok_Bool(n)) in
      (t, Bool(n))

    |Tok_ID(n)::t -> let t = match_token toks (Tok_ID(n)) in
      (t, ID(n))

    |Tok_LParen::t -> (let t = match_token toks Tok_LParen in
        let t2, e2 = parse_expr t in
          match t2 with
            |Tok_RParen::t -> t, e2
            |_ -> failwith "unbalanced parens")
    |_ -> raise (InvalidInputException "invalid")

let rec parse_stmt toks : stmt_result = match lookahead toks with

  |Tok_Int_Type -> let t1, e1 = parse_declare toks in
    (*recurse over the rest of the list, put tok for e1 and continue over stmt*)
    let t2, stmt = parse_stmt t1 in t2, Seq(e1, stmt)

  |Tok_Bool_Type -> let t1, e1 = parse_declare toks in
    let t2, stmt = parse_stmt t1 in t2, Seq(e1, stmt)

  |Tok_ID(id) -> let t1, e1 = parse_assign toks in
    let t2, stmt = parse_stmt t1 in t2, Seq(e1, stmt)

  |Tok_Print -> let t1, e1 = parse_print toks in
    let t2, stmt = parse_stmt t1 in t2, Seq(e1, stmt)

  |Tok_If -> let t1, e1 = parse_if toks in
    let t2, stmt = parse_stmt t1 in t2, Seq(e1, stmt)

  |Tok_For -> let t1, e1 = parse_for toks in
    let t2, stmt = parse_stmt t1 in t2, Seq(e1, stmt)

  |Tok_While -> let t1, e1 = parse_while toks in
    let t2, stmt = parse_stmt t1 in t2, Seq(e1, stmt)

  |_ -> (toks, NoOp)

  and parse_declare toks = match lookahead toks with
    |Tok_Int_Type -> (let t1 = match_token toks Tok_Int_Type in
        (match lookahead t1 with
          |Tok_ID(name) -> (*match name of variable*)
            let t2 = match_token t1 (Tok_ID(name)) in 
            let t3 = match_token t2 Tok_Semi in (t3, Declare(Int_Type, name))
          |_ -> failwith ("error!")))
    
    |Tok_Bool_Type -> (let t1 = match_token toks Tok_Bool_Type in
        (match lookahead t1 with
          |Tok_ID(name) ->
            let t2 = match_token t1 (Tok_ID(name)) in
            let t3 = match_token t2 Tok_Semi in (t3, Declare(Bool_Type, name))
          |_ -> failwith ("error!")))
    |_ -> failwith ("error!")

  and parse_assign toks = match lookahead toks with
    |Tok_ID(id) -> let t1 = match_token toks (Tok_ID(id)) in
      let t2 = match_token t1 Tok_Assign in 
      let t3, e1 = parse_expr t2 in
      let t4 = match_token t3 Tok_Semi in (t4, Assign(id, e1))
    |_ -> failwith ("error")

  and parse_print toks = match lookahead toks with
    |Tok_Print -> let t1 = match_token toks Tok_Print in 
      let t2 = match_token t1 Tok_LParen in 
      let t3, e1 = parse_expr t2 in
      let t4 = match_token t3 Tok_RParen in
      let t5 = match_token t4 Tok_Semi in (t5, Print(e1))
    |_ -> failwith ("error")

  and parse_if toks = match lookahead toks with
    |Tok_If -> let t1 = match_token toks Tok_If in 
      let t2 = match_token t1 Tok_LParen in 
      let t3, e1 = parse_expr t2 in
      let t4 = match_token t3 Tok_RParen in
      let t5 = match_token t4 Tok_LBrace in
      let t6, e2 = parse_stmt t5 in
      let t7 = match_token t6 Tok_RBrace in
          (match lookahead t7 with
            |Tok_Else -> let t8 = match_token t7 Tok_Else in 
                         let t9 = match_token t8 Tok_LBrace in
                         let t10, e3 = parse_stmt t9 in
                         let t11 = match_token t10 Tok_RBrace in
                          (t11, If(e1, e2, e3))
            |_ -> (t7, If(e1, e2, NoOp)))
    |_ -> failwith ("error!")

  and parse_for toks = match lookahead toks with
    |Tok_For -> let t1 = match_token toks Tok_For in
      let t2 = match_token t1 Tok_LParen in
        (match lookahead t2 with
          |Tok_ID(var) -> let t3 = match_token t2 (Tok_ID(var)) in
                  let t4 = match_token t3 Tok_From in
                  let t5, e1 = parse_expr t4 in
                  let t6 = match_token t5 Tok_To in
                  let t7, e2 = parse_expr t6 in
                  let t8 = match_token t7 Tok_RParen in
                  let t9 = match_token t8 Tok_LBrace in
                  let t10, e3 = parse_stmt t9 in
                  let t11 = match_token t10 Tok_RBrace in (t11, For(var, e1, e2, e3))
          |_ -> failwith ("error!"))
    |_ -> failwith ("error!")

  and parse_while toks = match lookahead toks with
    |Tok_While -> let t1 = match_token toks Tok_While in
      let t2 = match_token t1 Tok_LParen in
      let t3, e1 = parse_expr t2 in
      let t4 = match_token t3 Tok_RParen in 
      let t5 = match_token t4 Tok_LBrace in 
      let t6, e2 = parse_stmt t5 in
      let t7 = match_token t6 Tok_RBrace in (t7, While(e1, e2))
    |_ -> failwith ("error!")


let parse_main toks : stmt =
  let t1 = match_token toks Tok_Int_Type in
  let t2 = match_token t1 Tok_Main in 
  let t3 = match_token t2 Tok_LParen in
  let t4 = match_token t3 Tok_RParen in
  let t5 = match_token t4 Tok_LBrace in
  let t6, e1 = parse_stmt t5 in
  let t7 = match_token t6 Tok_RBrace in
    match lookahead t7 with
      |EOF -> e1
      |_ -> raise (InvalidInputException "invalid!")

