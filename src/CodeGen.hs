
    module CodeGen where


import LLVM.Core as Core
import LLVM.FFI.Core as FFI


import TypeNames
import Scanner


codegen :: Program -> IO String
codegen PEmpty = return "OK"
codegen (Program program funcdef) =
    do
        mod <- newNamedModule "__global_module__"

        codegenProgram (Program program funcdef) mod
        return "OK"

-- |
codegenProgram :: Program -> Core.Module -> IO String
codegenProgram (Program PEmpty funcdef) mod = 
    do
        codegenFunc funcdef
        return "OK"

codegenProgram (Program program funcdef) mod = 
    do
        codegenProgram program mod
        codegenFunc funcdef
        return "OK"

-- |
codegenFunc :: Funcdef -> IO String
codegenFunc (Funcdef name params stmts) = 
    do 
        
        codegenParams params
        codegenStmts stmts
        return "OK"

-- |
codegenParams :: Params -> IO String 
codegenParams (ParamsOne name) = 
    do 
        
        return "OK"

codegenParams (Params name params) =
    do 
        (codegenParams params)
        return "OK"

codegenParams (PaEmpty) =
    do
        return "OK"

codegenStmts :: Stmts -> IO String
codegenStmts (Stmts stmt stmts) =
    do 
        codegenStmt stmt
        codegenStmts stmts
        return "OK"

codegenStmts (SEmpty) =
    do 
        return "OK"

codegenStmt :: Stmt -> IO String
codegenStmt (StmtReturn name) =
    do 
        return "OK"

codegenStmt (StmtReturnNull) =
    do
        return "OK"

codegenStmt (StmtGoto name) =
    do
        return "OK"

codegenStmt (StmtIf condition stmts) = 
    do 
--        codegenExpr condition
        codegenStmts stmts
        return "OK"

--codegenStmt (StmtDecl name expr) = 
--    do
--        codegenExpr expr
--        return "OK"
--
--codegenStmt (StmtTerm term) =
--    do 
--        codegenTerm term
--
--
--
--codegenExpr :: Expr -> IO String
--codegenExpr (expr) =
--    do
--        retrun "OK"
--
--codegenTerm :: Term -> IO String
--codegenTerm (

