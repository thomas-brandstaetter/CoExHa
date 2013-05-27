
    module CodeGen where


import LLVM.Core as Core
import LLVM.FFI.Core as FFI
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Array (withArrayLen, withArray, allocaArray, peekArray)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Utils (fromBool)
import System.IO.Unsafe (unsafePerformIO)

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
        codegenFunc funcdef mod
        return "OK"

codegenProgram (Program program funcdef) mod = 
    do
        codegenProgram program mod
        codegenFunc funcdef mod
        return "OK"

-- |
codegenFunc :: Funcdef -> Core.Module -> IO String
codegenFunc (Funcdef name params stmts) mod = 
    do
        codegenParams params
        codegenStmts stmts
        return "OK"



-- | codegenParams 
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
        let methodType = funcType (False :: Bool) FFI.int64Type [FFI.int64Type]
        return "OK"


-- | getArgTypes is a helper in order to convert
-- myceh types into LLVM TypeRefs
paramTypes :: Params -> [FFI.TypeRef]
paramTypes PaEmpty = []
paramTypes (ParamsOne ident) = [FFI.int64Type]
paramTypes (Params ident ps) = 
    [FFI.int64Type] ++ (paramTypes ps)


-- | funcType is a helper to provide LLVM the correct types
--
funcType :: Bool -> FFI.TypeRef -> [FFI.TypeRef] -> FFI.TypeRef
funcType varargs retType paramTypes = unsafePerformIO $
    withArrayLen paramTypes $ \ len ptr ->
        return $ FFI.functionType retType ptr (fromIntegral len)
	       	 		  False

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

