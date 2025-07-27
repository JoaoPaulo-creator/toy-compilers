(* tokens.ml *)

type t =
  (* literals *)
  | IntLit of int
  | BoolLit of bool
  | Ident of string

  (* punctuation *)
  | Semicolon | Comma | Dot
  | LParen | RParen | LBrace | RBrace
  | LBracket | RBracket

  (* operators *)
  | Plus | Minus | Star | Slash
  | Bang              (* '!' *)
  | Equal             (* '=' *)
  | EqualEqual        (* '==' *)
  | NotEqual          (* '!=' *)
  | Less | LessEqual
  | Greater | GreaterEqual

  (* keywords *)
  | If | Else | While | For
  | Let
  | True | False
  | EOF



let to_string = function
  | IntLit i       -> Printf.sprintf "IntLit(%d)" i
  | BoolLit b      -> Printf.sprintf "BoolLit(%B)" b
  | Ident s        -> Printf.sprintf "Ident(%S)" s
  | Semicolon      -> "Semicolon"
  | Comma          -> "Comma"
  | Dot            -> "Dot"
  | LParen         -> "LParen"
  | RParen         -> "RParen"
  | LBrace         -> "LBrace"
  | RBrace         -> "RBrace"
  | LBracket       -> "LBracket"
  | RBracket       -> "RBracket"
  | Plus           -> "Plus"
  | Minus          -> "Minus"
  | Star           -> "Star"
  | Slash          -> "Slash"
  | Bang           -> "Bang"
  | Equal          -> "Equal"
  | EqualEqual     -> "EqualEqual"
  | NotEqual       -> "NotEqual"
  | Less           -> "Less"
  | LessEqual      -> "LessEqual"
  | Greater        -> "Greater"
  | GreaterEqual   -> "GreaterEqual"
  | If             -> "If"
  | Else           -> "Else"
  | While          -> "While"
  | For            -> "For"
  | Let            -> "Let"
  | True           -> "True"
  | False          -> "False"
  | EOF            -> "EOF"