open TokenTypes

let tokenize input =
  let rec make_token s = 
  let mk_compile s = Re.compile(Re.Perl.re s) in
  let lparen_re = mk_compile "^\\(" in
  let rparen_re = mk_compile "^\\)" in
  let lbrace_re = mk_compile "^\\{" in
  let rbrace_re = mk_compile "^\\}" in
  let equal_re = mk_compile "^==" in
  let n_equal_re = mk_compile "^!=" in
  let assign_re = mk_compile "^=" in
  let greater_re = mk_compile "^>" in
  let less_re = mk_compile "^<" in
  let greater_eq_re = mk_compile "^>=" in
  let less_eq_re = mk_compile "^<=" in
  let or_re = mk_compile "^\\|\\|" in
  let and_re = mk_compile "^&&" in
  let not_re = mk_compile "^!" in
  let semi_re = mk_compile "^;" in
  let int_tp_re = mk_compile "^int\\b" in
  let bool_tp_re = mk_compile "^bool\\b" in
  let print_re = mk_compile "^printf\\b" in
  let main_re = mk_compile "^main\\b" in
  let if_re = mk_compile "^if\\b" in
  let else_re = mk_compile "^else\\b" in
  let for_re = mk_compile "^for\\b" in
  let from_re = mk_compile "^from\\b" in
  let to_re = mk_compile "^to\\b" in
  let while_re = mk_compile "^while\\b" in
  let add_re = mk_compile "^\\+" in
  let sub_re = mk_compile "^\\-" in
  let mult_re = mk_compile "^\\*" in
  let div_re = mk_compile "^\\/" in
  let pow_re = mk_compile "^\\^" in
  let int_re = mk_compile "^-?[0-9]+" in
  let bool_re = mk_compile "^(true|false)\\b" in
  let id_re = mk_compile "^[a-zA-Z][a-zA-Z0-9]*" in
  let whitespace_re = mk_compile "^[ \t\n]+" in

      if s = "" then [EOF]
        else if Re.execp whitespace_re s then
          let space_len = String.length (Re.Group.get (Re.exec whitespace_re s) 0) in
            make_token (String.sub s space_len (String.length s - space_len))

        else if Re.execp equal_re s then
          Tok_Equal::(make_token (String.sub s 2 ((String.length s) - 2)))

        else if Re.execp n_equal_re s then
          Tok_NotEqual::(make_token (String.sub s 2 ((String.length s) - 2)))

        else if Re.execp greater_eq_re s then
          Tok_GreaterEqual::(make_token (String.sub s 2 ((String.length s) - 2)))

        else if Re.execp less_eq_re s then
          Tok_LessEqual::(make_token (String.sub s 2 ((String.length s) - 2)))

        else if Re.execp or_re s then
          Tok_Or::(make_token (String.sub s 2 ((String.length s) - 2)))

        else if Re.execp and_re s then
          Tok_And::(make_token (String.sub s 2 ((String.length s) - 2)))

        else if Re.execp lparen_re s then
          Tok_LParen::(make_token (String.sub s 1 ((String.length s) - 1)))

        else if Re.execp rparen_re s then
          Tok_RParen::(make_token (String.sub s 1 ((String.length s) - 1)))

        else if Re.execp lbrace_re s then
          Tok_LBrace::(make_token (String.sub s 1 ((String.length s) - 1)))

        else if Re.execp rbrace_re s then
          Tok_RBrace::(make_token (String.sub s 1 ((String.length s) - 1)))

        else if Re.execp assign_re s then
          Tok_Assign::(make_token (String.sub s 1 ((String.length s) - 1)))

        else if Re.execp greater_re s then
          Tok_Greater::(make_token (String.sub s 1 ((String.length s) - 1)))

        else if Re.execp less_re s then
          Tok_Less::(make_token (String.sub s 1 ((String.length s) - 1)))

        else if Re.execp not_re s then (*maybe need to move above not?*)
          Tok_Not::(make_token (String.sub s 1 ((String.length s) - 1)))

        else if Re.execp semi_re s then
          Tok_Semi::(make_token (String.sub s 1 ((String.length s) - 1)))

        else if Re.execp int_tp_re s then
          Tok_Int_Type::(make_token (String.sub s 3 ((String.length s) - 3)))

        else if Re.execp bool_tp_re s then
          Tok_Bool_Type::(make_token (String.sub s 4 ((String.length s) - 4)))

        else if Re.execp print_re s then
          Tok_Print::(make_token (String.sub s 6 ((String.length s) - 6)))

        else if Re.execp main_re s then
          Tok_Main::(make_token (String.sub s 4 ((String.length s) - 4)))

        else if Re.execp if_re s then
          Tok_If::(make_token (String.sub s 2 ((String.length s) - 2)))

        else if Re.execp else_re s then
          Tok_Else::(make_token (String.sub s 4 ((String.length s) - 4)))

        else if Re.execp for_re s then
          Tok_For::(make_token (String.sub s 3 ((String.length s) - 3)))

        else if Re.execp from_re s then
          Tok_From::(make_token (String.sub s 4 ((String.length s) - 4)))

        else if Re.execp to_re s then
          Tok_To::(make_token (String.sub s 2 ((String.length s) - 2)))

        else if Re.execp while_re s then
          Tok_While::(make_token (String.sub s 5 ((String.length s) - 5)))

        else if Re.execp bool_re s then
          let boolean = (Re.Group.get (Re.exec bool_re s)) 0 in
          let tok = if boolean = "true" then Tok_Bool(true) else Tok_Bool(false) in
          let len = String.length boolean in
            tok :: (make_token (String.sub s len (String.length s - len)))

        else if Re.execp int_re s then
          let str_int = (Re.Group.get (Re.exec int_re s)) 0 in
            Tok_Int(int_of_string(str_int)) :: (make_token (String.sub s (String.length str_int) (String.length s - String.length str_int)))

        else if Re.execp id_re s then
          let str = (Re.Group.get (Re.exec id_re s)) 0 in
            Tok_ID(str) :: (make_token (String.sub s (String.length str) (String.length s - String.length str)))

        else if Re.execp add_re s then
          Tok_Add::(make_token (String.sub s 1 ((String.length s) - 1)))

        else if Re.execp sub_re s then
          Tok_Sub::(make_token (String.sub s 1 ((String.length s) - 1)))

        else if Re.execp mult_re s then
          Tok_Mult::(make_token (String.sub s 1 ((String.length s) - 1)))

        else if Re.execp div_re s then
          Tok_Div::(make_token (String.sub s 1 ((String.length s) - 1)))

        else if Re.execp pow_re s then
          Tok_Pow::(make_token (String.sub s 1 ((String.length s) - 1)))

        else failwith ("not a valid word!") in make_token input
