
    module CodeGen where


import LLVM.Core as Core
import LLVM.FFI.Core as FFI
import LLVM.Wrapper.Core as Wrapper

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
        module_ <- Wrapper.moduleCreateWithName "__global_module__"

        codegenProgram (Program program funcdef) module_
        return "OK"

-- |
codegenProgram :: Program -> Wrapper.Module -> IO String
codegenProgram (Program PEmpty funcdef) module_ = 
    do
        codegenFunc funcdef module_
        return "OK"

codegenProgram (Program program funcdef) module_ = 
    do
        codegenProgram program module_
        codegenFunc funcdef module_
        return "OK"

-- |
codegenFunc :: Funcdef -> Wrapper.Module -> IO String
codegenFunc (Funcdef name params stmts) module_ = 
    do
        let methodType = Wrapper.functionType FFI.int64Type (paramTypes params) (False :: Bool)
        method <- Wrapper.addFunction module_ name methodType
        builder <- Wrapper.createBuilder
        entry <- Wrapper.appendBasicBlock method "entry"
        codegenStmts stmts builder
        return "OK"



-- | getArgTypes is a helper in order to convert
-- myceh types into LLVM TypeRefs
paramTypes :: Params -> [FFI.TypeRef]
paramTypes PaEmpty = [FFI.int64Type]            -- return 0 for null
paramTypes (ParamsOne ident) = [FFI.int64Type]
paramTypes (Params ident ps) = 
    [FFI.int64Type] ++ (paramTypes ps)


codegenStmts :: Stmts -> Wrapper.Builder -> IO String
codegenStmts (Stmts stmt stmts) builder =
    do 
        codegenStmt stmt builder
        codegenStmts stmts builder
        return "OK"

codegenStmts (SEmpty) builder =
    do        
        return "OK"

codegenStmt :: Stmt -> Wrapper.Builder -> IO String
codegenStmt (StmtReturn term) builder =
    do 
        return "OK"

codegenStmt (StmtReturnNull) builder =
    do
        Wrapper.buildRet builder (constInt FFI.int64Type 0 (False :: Bool))
        return "OK"

codegenStmt (StmtIf condition stmts) builder = 
    do 
--        codegenExpr condition
        codegenStmts stmts builder
        return "OK"

codegenStmt (StmtGoto _) builder =
    do
        error "not implemented: goto"



-- | codegenTerm
codegenTerm :: Term -> Wrapper.Builder -> IO String
codegenTerm (TermId name) builder = 
    do
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

