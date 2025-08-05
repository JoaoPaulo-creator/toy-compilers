module CodeGen where

import Data.List (intercalate)
import Parser

-- Generate x86_64 assembly code (Intel syntax)
generateCode :: Program -> String
generateCode (Program funcs) =
  let globalDecl = ".intel_syntax noprefix\n.global main\n\n"
      funcCode = concatMap genFunc funcs
      externs = "extern printf\nextern malloc\nextern free\n\n"
      dataSection = ".section .data\nformat_str: .asciz \"%d\\n\"\n"
   in globalDecl ++ externs ++ dataSection ++ ".section .text\n" ++ funcCode

-- Generate code for a single function
genFunc :: Func -> String
genFunc (Func name params retType stmts) =
  let setup =
        name
          ++ ":\n"
          ++ "  push rbp\n"
          ++ "  mov rbp, rsp\n"
          ++
          -- Allocate space for parameters
          concatMap (\(i, _) -> "  mov [rbp-" ++ show (8 * (i + 1)) ++ "], " ++ reg i ++ "\n") (zip [0 ..] params)
      body = concatMap genStmt stmts
      cleanup =
        "  mov rsp, rbp\n"
          ++ "  pop rbp\n"
          ++ "  ret\n\n"
   in setup ++ body ++ cleanup
  where
    reg i = case i of
      0 -> "rdi"
      1 -> "rsi"
      _ -> "rdx"

-- Generate code for a statement
genStmt :: Stmt -> String
genStmt (SDecl _ name expr) =
  -- Assuming variables are on stack
  genExpr expr
    ++ "  mov [rbp-"
    ++ name
    ++ "], rax\n"
genStmt (SAssign name expr) =
  genExpr expr
    ++ "  mov [rbp-"
    ++ name
    ++ "], rax\n"
genStmt (SArrayAssign arr index value) =
  genExpr index
    ++ "  mov rbx, rax\n"
    ++ genExpr value
    ++ "  mov rcx, rax\n"
    ++ genExpr arr
    ++ "  mov [rax + rbx*8], rcx\n"
genStmt (SIf cond thenStmts elseStmts) =
  let labelElse = "else_" ++ show (length thenStmts)
      labelEnd = "end_" ++ show (length thenStmts)
   in genExpr cond
        ++ "  cmp rax, 0\n"
        ++ "  je "
        ++ labelElse
        ++ "\n"
        ++ concatMap genStmt thenStmts
        ++ "  jmp "
        ++ labelEnd
        ++ "\n"
        ++ labelElse
        ++ ":\n"
        ++ concatMap genStmt elseStmts
        ++ labelEnd
        ++ ":\n"
genStmt (SWhile cond stmts) =
  let labelStart = "while_" ++ show (length stmts)
      labelEnd = "end_while_" ++ show (length stmts)
   in labelStart
        ++ ":\n"
        ++ genExpr cond
        ++ "  cmp rax, 0\n"
        ++ "  je "
        ++ labelEnd
        ++ "\n"
        ++ concatMap genStmt stmts
        ++ "  jmp "
        ++ labelStart
        ++ "\n"
        ++ labelEnd
        ++ ":\n"
genStmt (SFor var init cond update stmts) =
  let labelStart = "for_" ++ show (length stmts)
      labelEnd = "end_for_" ++ show (length stmts)
   in genExpr init
        ++ "  mov [rbp-"
        ++ var
        ++ "], rax\n"
        ++ labelStart
        ++ ":\n"
        ++ genExpr cond
        ++ "  cmp rax, 0\n"
        ++ "  je "
        ++ labelEnd
        ++ "\n"
        ++ concatMap genStmt stmts
        ++ genExpr update
        ++ "  mov [rbp-"
        ++ var
        ++ "], rax\n"
        ++ "  jmp "
        ++ labelStart
        ++ "\n"
        ++ labelEnd
        ++ ":\n"
genStmt (SPrint expr) =
  genExpr expr
    ++ "  mov rdi, offset format_str\n"
    ++ "  mov rsi, rax\n"
    ++ "  xor rax, rax\n"
    ++ "  call printf\n"
genStmt (SReturn expr) =
  genExpr expr
    ++ "  mov rsp, rbp\n"
    ++ "  pop rbp\n"
    ++ "  ret\n"
genStmt (SExpr expr) = genExpr expr

-- Generate code for an expression
genExpr :: Expr -> String
genExpr (EIntLit n) = "  mov rax, " ++ show n ++ "\n"
genExpr (EIdent name) = "  mov rax, [rbp-" ++ name ++ "]\n"
genExpr (EBinOp op left right) =
  genExpr right
    ++ "  push rax\n"
    ++ genExpr left
    ++ "  pop rbx\n"
    ++ case op of
      "+" -> "  add rax, rbx\n"
      "-" -> "  sub rax, rbx\n"
      "*" -> "  imul rax, rbx\n"
      "==" ->
        "  cmp rax, rbx\n"
          ++ "  sete al\n"
          ++ "  movzx rax, al\n"
      _ -> error "Unsupported operator"
genExpr (EUnOp "!" expr) =
  genExpr expr
    ++ "  cmp rax, 0\n"
    ++ "  sete al\n"
    ++ "  movzx rax, al\n"
genExpr (EArrayAccess arr index) =
  genExpr index
    ++ "  mov rbx, rax\n"
    ++ genExpr arr
    ++ "  mov rax, [rax + rbx*8]\n"
genExpr (EArrayLit exprs) =
  let len = length exprs
   in "  mov rdi, "
        ++ show (len * 8)
        ++ "\n"
        ++ "  call malloc\n"
        ++ concatMap
          ( \(i, e) ->
              genExpr e
                ++ "  mov [rax + "
                ++ show (i * 8)
                ++ "], rax\n"
          )
          (zip [0 ..] exprs)
genExpr (ECall name args) =
  concatMap
    ( \(i, e) ->
        genExpr e
          ++ "  mov "
          ++ reg i
          ++ ", rax\n"
    )
    (zip [0 ..] args)
    ++ "  call "
    ++ name
    ++ "\n"
  where
    reg i = case i of
      0 -> "rdi"
      1 -> "rsi"
      _ -> "rdx"
genExpr (ELength arr) =
  genExpr arr
    ++ "  mov rax, [rax - 8]\n" -- Assuming length stored before array start
