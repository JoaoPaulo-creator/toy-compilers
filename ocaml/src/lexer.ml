(* lexer.ml *)

open Tokens

let is_digit c =
  let code = Char.code c in 
  code >= Char.code '0' && code <= Char.code '9'

let is_alpha c =
  let code = Char.code c in 
  (code >= Char.code 'A' && code <= Char.code 'Z') ||
  (code >= Char.code 'a' && code <= Char.code 'z')

let is_alphanum c =
  is_alpha c || is_digit c


let tokenize (input : string) : t list =
  let len = String.length input in
  let rec lex i acc =
    if i >= len then List.rev (EOF :: acc)
    else
      match input.[i] with
      | ' ' | '\n' | '\t' -> lex (i+1) acc
      | '0'..'9' as c ->
          (* read full integer literal *)
          let j = ref i in
          while !j < len && is_digit input.[!j] do incr j done;
          let n = int_of_string (String.sub input i (!j - i)) in
          lex !j (IntLit n :: acc)
      | 'a'..'z' | 'A'..'Z' | '_' as c ->
          (* read identifier or keyword *)
          let j = ref i in
          while !j < len &&
                (is_alphanum input.[!j] || input.[!j] = '_')
          do incr j done;
          let id = String.sub input i (!j - i) in
          let tok = match id with
            | "if"    -> If | "else" -> Else
            | "while" -> While | "for"   -> For
            | "let"   -> Let
            | "true"  -> True | "false" -> False
            | _       -> Ident id
          in
          lex !j (tok :: acc)
      | '+' -> lex (i+1) (Plus :: acc)
      | '-' -> lex (i+1) (Minus :: acc)
      | '*' -> lex (i+1) (Star :: acc)
      | '/' -> lex (i+1) (Slash :: acc)
      | '!' ->
          if i+1 < len && input.[i+1] = '=' then
            lex (i+2) (NotEqual :: acc)
          else
            lex (i+1) (Bang :: acc)
      | '=' ->
          if i+1 < len && input.[i+1] = '=' then
            lex (i+2) (EqualEqual :: acc)
          else
            lex (i+1) (Equal :: acc)
      | '<' ->
          if i+1 < len && input.[i+1] = '=' then
            lex (i+2) (LessEqual :: acc)
          else
            lex (i+1) (Less :: acc)
      | '>' ->
          if i+1 < len && input.[i+1] = '=' then
            lex (i+2) (GreaterEqual :: acc)
          else
            lex (i+1) (Greater :: acc)
      | ';' -> lex (i+1) (Semicolon :: acc)
      | ',' -> lex (i+1) (Comma :: acc)
      | '.' -> lex (i+1) (Dot :: acc)
      | '(' -> lex (i+1) (LParen :: acc)
      | ')' -> lex (i+1) (RParen :: acc)
      | '{' -> lex (i+1) (LBrace :: acc)
      | '}' -> lex (i+1) (RBrace :: acc)
      | '[' -> lex (i+1) (LBracket :: acc)
      | ']' -> lex (i+1) (RBracket :: acc)
      | c ->
          failwith (Printf.sprintf "Unexpected character '%c' at %d" c i)
  in
  lex 0 []
