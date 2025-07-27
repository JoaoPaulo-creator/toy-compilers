(* parser.ml *)

open Printf
open Tokens
open Ast

exception ParseError of string

(*── Utility functions ───────────────────────────────────────────────────────*)
let peek = function [] -> EOF | t::_ -> t
let advance = function [] -> (EOF, []) | t::ts -> (t, ts)

let token_to_string = function
  | IntLit i       -> sprintf "IntLit(%d)" i
  | BoolLit b      -> sprintf "BoolLit(%B)" b
  | Ident s        -> sprintf "Ident(%S)" s
  | Semicolon      -> "Semicolon"    | Comma          -> "Comma"
  | Dot            -> "Dot"          | LParen         -> "LParen"
  | RParen         -> "RParen"       | LBrace         -> "LBrace"
  | RBrace         -> "RBrace"       | LBracket       -> "LBracket"
  | RBracket       -> "RBracket"     | Plus           -> "Plus"
  | Minus          -> "Minus"        | Star           -> "Star"
  | Slash          -> "Slash"        | Bang           -> "Bang"
  | Equal          -> "Equal"        | EqualEqual     -> "EqualEqual"
  | NotEqual       -> "NotEqual"     | Less           -> "Less"
  | LessEqual      -> "LessEqual"    | Greater        -> "Greater"
  | GreaterEqual   -> "GreaterEqual" | If             -> "If"
  | Else           -> "Else"         | While          -> "While"
  | For            -> "For"          | Let            -> "Let"
  | True           -> "True"         | False          -> "False"
  | EOF            -> "EOF"

let expect expected tokens =
  match tokens with
  | t::ts when t = expected -> ts
  | t::_ ->
      let msg = sprintf "Expected %s but got %s"
        (token_to_string expected) (token_to_string t)
      in raise (ParseError msg)
  | [] -> raise (ParseError "Unexpected end of input")

(*── Expression parsing (precedence climbing) ────────────────────────────────*)
let rec parse_primary tokens =
  match peek tokens with
  | IntLit n -> let _, tl = advance tokens in (Int n, tl)
  | BoolLit b -> let _, tl = advance tokens in (Bool b, tl)
  | Ident id -> let _, tl = advance tokens in (Var id, tl)
  | LParen ->
      let tl1 = expect LParen tokens in
      let (e, tl2) = parse_expr tl1 in
      let tl3 = expect RParen tl2 in
      (e, tl3)
  | LBracket ->
      let tl1 = expect LBracket tokens in
      let rec elements acc tl =
        match peek tl with
        | RBracket -> (List.rev acc, expect RBracket tl)
        | _ ->
            let (e, tl2) = parse_expr tl in
            let tl3 = if peek tl2 = Comma then expect Comma tl2 else tl2 in
            elements (e::acc) tl3
      in
      let (lst, tl') = elements [] tl1 in
      (ArrayLit lst, tl')
  | t -> raise (ParseError (sprintf "Expected primary but got %s" (token_to_string t)))

and parse_postfix (lhs, tokens) =
  match peek tokens with
  | LBracket ->
      let tl1 = expect LBracket tokens in
      let (idx, tl2) = parse_expr tl1 in
      let tl3 = expect RBracket tl2 in
      parse_postfix (Index(lhs, idx), tl3)
  | Dot ->
      let tl1 = expect Dot tokens in
      (match peek tl1 with
       | Ident name ->
           let tl2 = advance tl1 |> snd in
           if peek tl2 = LParen then begin
             let tl3 = expect LParen tl2 in
             let rec gather_args acc tl4 =
               match peek tl4 with
               | RParen -> (List.rev acc, expect RParen tl4)
               | _ ->
                   let (e, tl5) = parse_expr tl4 in
                   let tl6 = if peek tl5 = Comma then expect Comma tl5 else tl5 in
                   gather_args (e::acc) tl6
             in
             let (args, tl7) = gather_args [] tl3 in
             parse_postfix (Call(lhs, name, args), tl7)
           end else
             parse_postfix (Prop(lhs, name), tl2)
       | t -> raise (ParseError (sprintf "Expected identifier after '.' but got %s" (token_to_string t))))
  | _ -> (lhs, tokens)

and parse_unary tokens =
  match peek tokens with
  | Bang -> let _, tl1 = advance tokens in let (e, tl2) = parse_unary tl1 in (Unary(Not, e), tl2)
  | _ -> let (p, tl) = parse_primary tokens in parse_postfix (p, tl)

and parse_binary_rhs expr_prec lhs tokens =
  let rec loop lhs tokens =
    let (prec, _) = match peek tokens with
      | Star|Slash -> (40,false)
      | Plus|Minus -> (30,false)
      | EqualEqual|NotEqual|Less|LessEqual|Greater|GreaterEqual -> (20,false)
      | _ -> (-1,false)
    in
    if prec < expr_prec then (lhs,tokens) else
    let (op, tl1) = advance tokens in
    let (rhs, tl2) = parse_unary tl1 in
    let (rhs2, tl3) = loop rhs tl2 in
    let binop = match op with
      | Plus        -> Add | Minus    -> Sub
      | Star        -> Mul | Slash    -> Div
      | EqualEqual  -> Eq  | NotEqual -> Neq
      | Less        -> Lt  | LessEqual-> Le
      | Greater     -> Gt  | GreaterEqual-> Ge
      | _ -> assert false
    in loop (Binary(lhs,binop,rhs2)) tl3
  in loop lhs tokens

and parse_expr tokens = let (lhs,tl) = parse_unary tokens in parse_binary_rhs 0 lhs tl

(*── Statement & block parsing ────────────────────────────────────────────────*)
and parse_stmt tokens =
  match (peek tokens, tokens) with

  (* print expr; *)
  | (Ident s, ts) when s = "print" ->
      let _, tl1 = advance ts in
      let (arg, tl2) = parse_expr tl1 in
      let tl3 = expect Semicolon tl2 in
      (ExprStmt (Call(arg, "print", [])), tl3)

  (* assignment: x = expr; *)
  | (Ident id, _ :: Equal :: _) ->
      let _, tl1 = advance tokens in
      let tl2 = expect Equal tl1 in
      let (rhs, tl3) = parse_expr tl2 in
      let tl4 = expect Semicolon tl3 in
      (Assign(id, rhs), tl4)

  (* let x = expr; *)
  | (Let, Let :: Ident s :: _) ->
      let _, tl1 = advance tokens in
      let name = s in
      let tl2 = advance tl1 |> snd in
      let tl3 = expect Equal tl2 in
      let (rhs, tl4) = parse_expr tl3 in
      let tl5 = expect Semicolon tl4 in
      (Let(name, rhs), tl5)

  (* if (cond) { then } [ else { else } ] *)
  | (If, _) ->
      let _, tl1 = advance tokens in
      let tl2 = expect LParen tl1 in
      let (cond, tl3) = parse_expr tl2 in
      let tl4 = expect RParen tl3 in
      let (then_blk, tl5) = parse_block tl4 in
      let (else_blk_opt, tl6) =
        if peek tl5 = Else then
          let _, tl7 = advance tl5 in
          let (blk, tl8) = parse_block tl7 in
          (Some blk, tl8)
        else
          (None, tl5)
      in
      (If(cond, then_blk, else_blk_opt), tl6)

  (* while (cond) { body } *)
  | (While, _) ->
      let _, tl1 = advance tokens in
      let tl2 = expect LParen tl1 in
      let (cond, tl3) = parse_expr tl2 in
      let tl4 = expect RParen tl3 in
      let (body, tl5) = parse_block tl4 in
      (While(cond, body), tl5)

  (* for (init; cond; post) { body } *)
  | (For, _) ->
      let _, tl1 = advance tokens in
      let tl2 = expect LParen tl1 in
      (* init *)
      let init_opt, tl3 =
        if peek tl2 = Semicolon then (None, advance tl2 |> snd)
        else let (st, t') = parse_stmt tl2 in (Some st, t')
      in
      (* cond *)
      let cond_opt, tl4 =
        if peek tl3 = Semicolon then (None, advance tl3 |> snd)
        else let (e, t') = parse_expr tl3 in (Some e, expect Semicolon t')
      in
      (* post without semicolon *)
      let post_opt, tl5 =
        if peek tl4 = RParen then (None, tl4)
        else
          let post, tl_after =
            match peek tl4 with
            | Ident id ->
                let tl_a = advance tl4 |> snd in
                let tl_b = expect Equal tl_a in
                let (rhs, tl_c) = parse_expr tl_b in
                (Assign(id, rhs), tl_c)
            | _ ->
                let (e, tl_c) = parse_expr tl4 in
                (ExprStmt e, tl_c)
          in
          (Some post, tl_after)
      in
      let tl6 = expect RParen tl5 in
      let (body, tl7) = parse_block tl6 in
      (For(init_opt, cond_opt, post_opt, body), tl7)

  (* block: { stmts } *)
  | (LBrace, _) ->
      let (blk, tl') = parse_block tokens in
      (Block blk, tl')

  (* expr; *)
  | _ ->
      let (e, tl1) = parse_expr tokens in
      let tl2 = expect Semicolon tl1 in
      (ExprStmt e, tl2)

and parse_block tokens =
  let tl1 = expect LBrace tokens in
  let rec loop tl acc =
    match peek tl with
    | RBrace -> let _, tl2 = advance tl in (List.rev acc, tl2)
    | EOF -> raise (ParseError "Unclosed block")
    | _ -> let (st, tl') = parse_stmt tl in loop tl' (st::acc)
  in loop tl1 []

and parse_program tokens =
  let rec loop tl acc =
    match peek tl with
    | EOF -> List.rev acc
    | _ -> let (st, tl') = parse_stmt tl in loop tl' (st::acc)
  in loop tokens []