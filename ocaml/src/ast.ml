(* ast.ml *)

type binop =
  | Add | Sub | Mul | Div
  | Eq | Neq | Lt | Le | Gt | Ge

type unop =
  | Not

type expr =
  | Int    of int
  | Bool   of bool
  | Var    of string
  | Binary of expr * binop * expr
  | Unary  of unop  * expr
  | ArrayLit of expr list
  | Index    of expr * expr           (* arr[index] *)
  | Prop     of expr * string         (* arr.length *)
  | Call     of expr * string * expr list

type stmt =
  | Let      of string * expr
  | Assign   of string * expr
  | ExprStmt of expr
  | If       of expr * stmt list * stmt list option
  | While    of expr * stmt list
  | For      of stmt option * expr option * stmt option * stmt list
  | Block    of stmt list

type program = stmt list
