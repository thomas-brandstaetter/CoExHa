
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


-- | codegenExpr
codegenExpr :: Expr -> Wrapper.Builder -> IO Wrapper.Value
codegenExpr (ExprUnary unary) builder = 
    do
        val <- codegenUnary unary builder
        return val

codegenExpr (ExprPlus lterm rterm) builder =
    do
        lval <- codegenTerm lterm builder
        rval <- codegenTerm rterm builder
        ret <- Wrapper.buildAnd builder lval rval "tmpand"
        return ret


codegenExpr (ExprMult lterm rterm) builder =
    do
        lval <- codegenTerm lterm builder
        rval <- codegenTerm rterm builder
        ret <- Wrapper.buildMul builder lval rval "tmpmul"
        return ret

codegenExpr (ExprAnd lterm rterm) builder =
    do
        lval <- codegenTerm lterm builder
        rval <- codegenTerm rterm builder
        ret <- Wrapper.buildAnd builder lval rval "tmpand"
        return ret

codegenExpr (ExprTerm term) builder =
    do
        val <- codegenTerm term builder
        return val



-- | codegenLExpr
codegenLExpr :: LExpr -> Wrapper.Builder -> IO String
codegenLExpr _ _ =
    do
        error "not implemented: lexpr"


-- | codegenUnary
codegenUnary :: Unary -> Wrapper.Builder -> IO Wrapper.Value
codegenUnary (UnaryNot value) builder =
    do
        val <- codegenUnary value builder
        ret <- Wrapper.buildNot builder val "tmpnot"
        return ret

codegenUnary (UnaryMinus value) builder =
    do
        val <- codegenUnary value builder
        ret <- Wrapper.buildNeg builder val "tmpminus"
        return ret

codegenUnary (UnaryTerm term) builder =
    do
        val <- codegenTerm term builder
        return val


-- | codegenTerm
codegenTerm :: Term -> Wrapper.Builder -> IO Wrapper.Value
codegenTerm (TermId name) builder = 
    do
        error "not implemented: Term id"

codegenTerm (TermExpr expr) builder = 
    do
        val <- codegenExpr expr builder
        return val

codegenTerm (TermNum num) builder = 
    do
        -- Wrapper.buildRet builder (constInt FFI.int64Type (fromIntegral num) (False :: Bool))
        error "not implemented: term num"

codegenTerm _ builder =
    do
        error "not implemented: call"



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

