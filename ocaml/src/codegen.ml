(* codegen.ml *)

open Printf
open Ast

(* Single buffer collecting all the generated assembly *)
let buf = Buffer.create 8192

(* Environment for local variables: name -> stack offset *)
let var_env : (string, int) Hashtbl.t = Hashtbl.create 16
let next_offset = ref 0    (* each local consumes 8 bytes *)


let buf = Buffer.create 8192
let var_env : (string, int) Hashtbl.t = Hashtbl.create 16
let next_offset = ref 0
let label_counter = ref 0
let array_literals = ref []

let label_counter = ref 0
let fresh_label prefix =
  incr label_counter;
  prefix ^ string_of_int !label_counter

(* Emit a line of assembly into [buf] *)
let emit fmt =
  ksprintf (fun line ->
    Buffer.add_string buf line;
    Buffer.add_char buf '\n'
  ) fmt

(* Compile an expression so that its result ends up in RAX *)
let rec compile_expr = function
  | Int n ->
      emit "  mov rax, %d" n

  | Bool b ->
      emit "  mov rax, %d" (if b then 1 else 0)

  | Var name ->
      let off = Hashtbl.find var_env name in
      emit "  mov rax, [rbp - %d]" off

  | Binary (lhs, op, rhs) ->
      compile_expr lhs;
      emit "  push rax";
      compile_expr rhs;
      emit "  mov rbx, rax";
      emit "  pop rax";
      let instr = match op with
        | Add -> "add rax, rbx"
        | Sub -> "sub rax, rbx"
        | Mul -> "imul rax, rbx"
        | Div -> "cqo\n  idiv rbx"
        | Eq  -> "cmp rax, rbx\n  sete al\n  movzx rax, al"
        | Neq -> "cmp rax, rbx\n  setne al\n  movzx rax, al"
        | Lt  -> "cmp rax, rbx\n  setl al\n  movzx rax, al"
        | Le  -> "cmp rax, rbx\n  setle al\n  movzx rax, al"
        | Gt  -> "cmp rax, rbx\n  setg al\n  movzx rax, al"
        | Ge  -> "cmp rax, rbx\n  setge al\n  movzx rax, al"
      in
      emit "  %s" instr

  | Unary (Not, e) ->
      compile_expr e;
      emit "  cmp rax, 0";
      emit "  sete al";
      emit "  movzx rax, al"

  | Call (receiver, "print", []) ->
      compile_expr receiver;
      emit "  mov rdi, fmt_int";
      emit "  mov rsi, rax";
      emit "  xor rax, rax";       (* for variadic printf ABI *)
      emit "  call printf"

  | Prop (array_expr, "length") ->
      compile_expr array_expr;
      emit "  mov rax, [rax]"     (* first qword holds length *)

  | ArrayLit elems ->
      let lbl = fresh_label "arr" in
      let values = List.map (function
        | Int n  -> string_of_int n
        | Bool b -> if b then "1" else "0"
        | _      -> failwith "ArrayLit: only constant elements supported"
      ) elems in
      let all = String.concat ", " (string_of_int (List.length elems) :: values) in
      (* emit "%s_data: dq %s" lbl all; *)
      array_literals := (lbl, all) :: !array_literals;
      emit "  lea rax, [%s_data]" lbl

  | Index (arr, idx) ->
      compile_expr arr;
      emit "  mov rbx, rax";
      compile_expr idx;
      emit "  mov rcx, rax";
      (* skip the length qword: element i is at offset 8 + i*8 *)
      emit "  mov rax, [rbx + rcx*8 + 8]"

  | Call _ ->
      failwith "Unsupported call in codegen"

(* Compile a statement *)
let rec compile_stmt = function

  | Let (name, expr) ->
      compile_expr expr;
      next_offset := !next_offset + 8;
      Hashtbl.add var_env name !next_offset;
      emit "  mov [rbp - %d], rax" !next_offset

  | Assign (name, expr) ->
      compile_expr expr;
      let off = Hashtbl.find var_env name in
      emit "  mov [rbp - %d], rax" off

  | ExprStmt e ->
      compile_expr e

  | If (cond, then_blk, else_blk_opt) ->
      let lbl_else = fresh_label "Lelse" in
      let lbl_end  = fresh_label "Lend" in
      compile_expr cond;
      emit "  cmp rax, 0";
      emit "  je %s" lbl_else;
      List.iter compile_stmt then_blk;
      emit "  jmp %s" lbl_end;
      emit "%s:" lbl_else;
      Option.iter (List.iter compile_stmt) else_blk_opt;
      emit "%s:" lbl_end

  | While (cond, body) ->
      let lbl_start = fresh_label "Lwhile" in
      let lbl_end   = fresh_label "Lwend" in
      emit "%s:" lbl_start;
      compile_expr cond;
      emit "  cmp rax, 0";
      emit "  je %s" lbl_end;
      List.iter compile_stmt body;
      emit "  jmp %s" lbl_start;
      emit "%s:" lbl_end

  | For (init_opt, cond_opt, post_opt, body) ->
      let lbl_start = fresh_label "Lfor" in
      let lbl_end   = fresh_label "Lfend" in
      Option.iter compile_stmt init_opt;
      emit "%s:" lbl_start;
      Option.iter (fun e -> compile_expr e; emit "  cmp rax, 0"; emit "  je %s" lbl_end) cond_opt;
      List.iter compile_stmt body;
      Option.iter compile_stmt post_opt;
      emit "  jmp %s" lbl_start;
      emit "%s:" lbl_end

  | Block stmts ->
      List.iter compile_stmt stmts

(* Top-level generate function *)
let generate prog =
  (* Reset array literals for each generation *)
  array_literals := [];
  
  (* TEXT SECTION *)
  emit "section .text";
  emit "global main";
  emit "extern printf\n";
  emit "main:";
  emit "  push rbp";
  emit "  mov rbp, rsp";
  emit "  sub rsp, 1024";
  
  (* Compile program *)
  List.iter compile_stmt prog;
  
  (* EPILOGUE *)
  emit "  mov rsp, rbp";
  emit "  pop rbp";
  emit "  mov rax, 0";
  emit "  ret\n";
  
  (* DATA SECTION (after text) *)
  emit "section .data";
  emit "fmt_int: db \"%%d\", 10, 0";  (* Moved here *)
  
  (* Emit all collected array literals *)
  List.iter (fun (lbl, data) ->
    emit "%s_data: dq %s" lbl data
  ) (List.rev !array_literals);
  
  Buffer.contents buf