module Parser where

import Lexer

-- AST data types
data Type = TInt | TBool | TArray Type | TVoid deriving (Show, Eq)

data Expr
  = EIntLit Int
  | EIdent String
  | EBinOp String Expr Expr
  | EUnOp String Expr
  | EArrayAccess Expr Expr
  | EArrayLit [Expr]
  | ECall String [Expr]
  | ELength Expr
  deriving (Show, Eq)

data Stmt
  = SAssign String Expr
  | SArrayAssign Expr Expr Expr
  | SDecl Type String Expr
  | SIf Expr [Stmt] [Stmt]
  | SWhile Expr [Stmt]
  | SFor String Expr Expr Expr [Stmt]
  | SPrint Expr
  | SReturn Expr
  | SExpr Expr
  | SDeclArray Type String Expr -- Added support for array declarations
  deriving (Show, Eq)

data Func = Func String [(Type, String)] Type [Stmt] deriving (Show, Eq)

data Program = Program [Func] deriving (Show, Eq)

-- Parser helper to match tokens
expect :: Token -> [Token] -> Either String [Token]
expect t (t' : ts) | t == t' = Right ts
expect t _ = Left $ "Expected " ++ show t

-- Main parser function
parse :: [Token] -> Either String Program
parse ts = do
  (funcs, _) <- parseFuncs ts
  return $ Program funcs

parseFuncs :: [Token] -> Either String ([Func], [Token])
parseFuncs ts@(TEOF : _) = Right ([], ts)
parseFuncs ts = do
  (func, ts') <- parseFunc ts
  (funcs, ts'') <- parseFuncs ts'
  return (func : funcs, ts'')

parseFunc :: [Token] -> Either String (Func, [Token])
parseFunc (TKeyword "func" : TIdent name : TLParen : ts) = do
  (params, ts') <- parseParams ts
  (retType, ts'') <- case ts' of
    TLBrace : _ -> Right (TVoid, ts') -- No return type, assume void
    _ -> parseType ts' -- Parse explicit return type
  ts''' <- expect TLBrace ts''
  (stmts, ts'''') <- parseStmts ts'''
  ts''''' <- expect TRBrace ts''''
  return (Func name params retType stmts, ts''''')
parseFunc _ = Left "Expected function declaration"

parseParams :: [Token] -> Either String ([(Type, String)], [Token])
parseParams (TRParen : ts) = Right ([], ts)
parseParams ts = do
  (ty, ts') <- parseType ts
  ts'' <- case ts' of
    TIdent name : ts'' -> Right ts''
    _ -> Left "Expected parameter name"
  let param = (ty, case ts' of TIdent n : _ -> n; _ -> "")
  ts''' <- case ts'' of
    TComma : ts''' -> Right ts'''
    TRParen : _ -> Right ts''
    _ -> Left "Expected comma or )"
  (params, ts'''') <- parseParams ts'''
  return (param : params, ts'''')

parseType :: [Token] -> Either String (Type, [Token])
parseType (TKeyword "int" : TLBracket : TRBracket : ts) = Right (TArray TInt, ts)
parseType (TKeyword "int" : ts) = Right (TInt, ts)
parseType (TKeyword "bool" : ts) = Right (TBool, ts)
parseType _ = Left "Expected type"

parseStmts :: [Token] -> Either String ([Stmt], [Token])
parseStmts ts@(TRBrace : _) = Right ([], ts)
parseStmts ts = do
  (stmt, ts') <- parseStmt ts
  (stmts, ts'') <- parseStmts ts'
  return (stmt : stmts, ts'')

parseStmt :: [Token] -> Either String (Stmt, [Token])
parseStmt (TKeyword "int" : TIdent name : TOperator "=" : ts) = do
  (expr, ts') <- parseExpr ts
  ts'' <- expect TSemicolon ts'
  return (SDecl TInt name expr, ts'')
parseStmt (TKeyword "bool" : TIdent name : TOperator "=" : ts) = do
  (expr, ts') <- parseExpr ts
  ts'' <- expect TSemicolon ts'
  return (SDecl TBool name expr, ts'')
parseStmt (TKeyword "int" : TLBracket : TRBracket : TIdent name : TOperator "=" : ts) = do
  (expr, ts') <- parseExpr ts
  ts'' <- expect TSemicolon ts'
  return (SDeclArray (TArray TInt) name expr, ts'')
parseStmt (TIdent name : TOperator "=" : ts) = do
  (expr, ts') <- parseExpr ts
  ts'' <- expect TSemicolon ts'
  return (SAssign name expr, ts'')
parseStmt (TIdent name : TLBracket : ts) = do
  (index, ts') <- parseExpr ts
  ts'' <- expect TRBracket ts'
  ts''' <- expect (TOperator "=") ts''
  (value, ts'''') <- parseExpr ts'''
  ts''''' <- expect TSemicolon ts''''
  return (SArrayAssign (EIdent name) index value, ts''''')
parseStmt (TKeyword "if" : TLParen : ts) = do
  (cond, ts') <- parseExpr ts
  ts'' <- expect TRParen ts'
  ts''' <- expect TLBrace ts''
  (thenStmts, ts'''') <- parseStmts ts'''
  ts''''' <- expect TRBrace ts''''
  (elseStmts, ts'''''') <- case ts''''' of
    TKeyword "else" : TLBrace : ts'''''' -> do
      (stmts, ts''''''') <- parseStmts ts''''''
      ts'''''''' <- expect TRBrace ts'''''''
      return (stmts, ts'''''''')
    _ -> Right ([], ts''''')
  return (SIf cond thenStmts elseStmts, ts'''''')
parseStmt (TKeyword "while" : TLParen : ts) = do
  (cond, ts') <- parseExpr ts
  ts'' <- expect TRParen ts'
  ts''' <- expect TLBrace ts''
  (stmts, ts'''') <- parseStmts ts'''
  ts''''' <- expect TRBrace ts''''
  return (SWhile cond stmts, ts''''')
parseStmt (TKeyword "for" : TLParen : TKeyword "int" : TIdent var : TOperator "=" : ts) = do
  (init, ts') <- parseExpr ts
  ts'' <- expect TSemicolon ts'
  (cond, ts''') <- parseExpr ts''
  ts'''' <- expect TSemicolon ts'''
  (update, ts''''') <- parseExpr ts''''
  ts'''''' <- expect TRParen ts'''''
  ts''''''' <- expect TLBrace ts''''''
  (stmts, ts''''''''') <- parseStmts ts'''''''
  ts'''''''''' <- expect TRBrace ts'''''''''
  return (SFor var init cond update stmts, ts'''''''''')
parseStmt (TKeyword "print" : TLParen : ts) = do
  (expr, ts') <- parseExpr ts
  ts'' <- expect TRParen ts'
  ts''' <- expect TSemicolon ts''
  return (SPrint expr, ts''')
parseStmt (TKeyword "return" : ts) = do
  (expr, ts') <- parseExpr ts
  ts'' <- expect TSemicolon ts'
  return (SReturn expr, ts'')
parseStmt ts = do
  (expr, ts') <- parseExpr ts
  ts'' <- expect TSemicolon ts'
  return (SExpr expr, ts'')

parseExpr :: [Token] -> Either String (Expr, [Token])
parseExpr (TIntLit n : ts) = Right (EIntLit (read n), ts)
parseExpr (TKeyword "true" : ts) = Right (EIntLit 1, ts)
parseExpr (TKeyword "false" : ts) = Right (EIntLit 0, ts)
parseExpr (TIdent name : TLParen : ts) = do
  (args, ts') <- parseArgs ts
  return (ECall name args, ts')
parseExpr (TIdent name : TLBracket : ts) = do
  (index, ts') <- parseExpr ts
  ts'' <- expect TRBracket ts'
  return (EArrayAccess (EIdent name) index, ts'')
parseExpr (TIdent name : TDot : TIdent "length" : ts) = Right (ELength (EIdent name), ts)
parseExpr (TIdent name : ts) = Right (EIdent name, ts)
parseExpr (TLBracket : ts) = do
  (exprs, ts') <- parseArrayLit ts
  return (EArrayLit exprs, ts')
parseExpr (TOperator "!" : ts) = do
  (expr, ts') <- parseExpr ts
  return (EUnOp "!" expr, ts')
parseExpr (TLParen : ts) = do
  (expr, ts') <- parseExpr ts
  ts'' <- expect TRParen ts'
  return (expr, ts'')
parseExpr ts = do
  (left, ts') <- parseSimpleExpr ts
  case ts' of
    TOperator op : ts'' -> do
      (right, ts''') <- parseExpr ts''
      return (EBinOp op left right, ts''')
    _ -> Right (left, ts')

parseSimpleExpr :: [Token] -> Either String (Expr, [Token])
parseSimpleExpr (TIntLit n : ts) = Right (EIntLit (read n), ts)
parseSimpleExpr (TKeyword "true" : ts) = Right (EIntLit 1, ts)
parseSimpleExpr (TKeyword "false" : ts) = Right (EIntLit 0, ts)
parseSimpleExpr (TIdent name : ts) = Right (EIdent name, ts)
parseSimpleExpr (TLParen : ts) = do
  (expr, ts') <- parseExpr ts
  ts'' <- expect TRParen ts'
  return (expr, ts'')
parseSimpleExpr _ = Left "Expected expression"

parseArrayLit :: [Token] -> Either String ([Expr], [Token])
parseArrayLit (TRBracket : ts) = Right ([], ts)
parseArrayLit ts = do
  (expr, ts') <- parseExpr ts
  case ts' of
    TComma : ts'' -> do
      (exprs, ts''') <- parseArrayLit ts''
      return (expr : exprs, ts''')
    TRBracket : ts'' -> Right ([expr], ts'')
    _ -> Left "Expected comma or ]"

parseArgs :: [Token] -> Either String ([Expr], [Token])
parseArgs (TRParen : ts) = Right ([], ts)
parseArgs ts = do
  (expr, ts') <- parseExpr ts
  case ts' of
    TComma : ts'' -> do
      (exprs, ts''') <- parseArgs ts''
      return (expr : exprs, ts''')
    TRParen : ts'' -> Right ([expr], ts'')
    _ -> Left "Expected comma or )"
