
    module CodeGen where


import LLVM.Core as Core
import LLVM.FFI.Core as FFI
import LLVM.Wrapper.Core as Wrapper
import LLVM.Wrapper.BitWriter as BitWriter

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Array (withArrayLen, withArray, allocaArray, peekArray)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Utils (fromBool)
import System.IO.Unsafe (unsafePerformIO)

import Data.Maybe

import TypeNames
import Scanner


codegen :: Program -> IO String
codegen PEmpty = return "OK"
codegen (Program program funcdef) =
    do
        module_ <- Wrapper.moduleCreateWithName "__global_module__"
        codegenProgram (Program program funcdef) module_
        BitWriter.writeBitcodeToFile module_ "bitcode.ir"
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
        codegenStmts stmts builder module_
        return "OK"



-- | getArgTypes is a helper in order to convert
-- myceh types into LLVM TypeRefs
paramTypes :: Params -> [FFI.TypeRef]
paramTypes PaEmpty = [FFI.int64Type]            -- return 0 for null
paramTypes (ParamsOne ident) = [FFI.int64Type]
paramTypes (Params ident ps) = 
    [FFI.int64Type] ++ (paramTypes ps)


codegenStmts :: Stmts -> Wrapper.Builder -> Wrapper.Module -> IO String
codegenStmts (Stmts stmt stmts) builder m =
    do 
        codegenStmt stmt builder m
        codegenStmts stmts builder m
        return "OK"

codegenStmts (SEmpty) builder m =
    do        
        return "OK"

codegenStmt :: Stmt -> Wrapper.Builder -> Wrapper.Module -> IO String
codegenStmt (StmtReturn term) builder m =
    do 
        return "OK"

codegenStmt (StmtReturnNull) builder m =
    do
        Wrapper.buildRet builder (constInt FFI.int64Type 0 (False :: Bool))
        return "OK"

codegenStmt (StmtIf condition stmts) builder m = 
    do 
--        codegenExpr condition
        codegenStmts stmts builder m
        return "OK"

codegenStmt (StmtGoto _) builder m =
    do
        error "not implemented: goto"


-- | codegenExpr
codegenExpr :: Expr -> Wrapper.Builder -> Wrapper.Module -> IO Wrapper.Value
codegenExpr (ExprUnary unary) builder m = 
    do
        val <- codegenUnary unary builder m
        return val

codegenExpr (ExprPlus lterm rterm) builder m =
    do
        lval <- codegenTerm lterm builder m
        rval <- codegenTerm rterm builder m
        ret <- Wrapper.buildAnd builder lval rval "tmpand"
        return ret


codegenExpr (ExprMult lterm rterm) builder m =
    do
        lval <- codegenTerm lterm builder m
        rval <- codegenTerm rterm builder m
        ret <- Wrapper.buildMul builder lval rval "tmpmul"
        return ret

codegenExpr (ExprAnd lterm rterm) builder m =
    do
        lval <- codegenTerm lterm builder m
        rval <- codegenTerm rterm builder m
        ret <- Wrapper.buildAnd builder lval rval "tmpand"
        return ret

codegenExpr (ExprTerm term) builder m =
    do
        val <- codegenTerm term builder m
        return val



-- | codegenLExpr
codegenLExpr :: LExpr -> Wrapper.Builder -> Wrapper.Module -> IO String
codegenLExpr _ _ _ =
    do
        error "not implemented: lexpr"


-- | codegenUnary
codegenUnary :: Unary -> Wrapper.Builder -> Wrapper.Module -> IO Wrapper.Value
codegenUnary (UnaryNot value) builder m =
    do
        val <- codegenUnary value builder m
        ret <- Wrapper.buildNot builder val "tmpnot"
        return ret

codegenUnary (UnaryMinus value) builder m =
    do
        val <- codegenUnary value builder m
        ret <- Wrapper.buildNeg builder val "tmpminus"
        return ret

codegenUnary (UnaryTerm term) builder m =
    do
        val <- codegenTerm term builder m
        return val


-- | codegenTerm
codegenTerm :: Term -> Wrapper.Builder -> Wrapper.Module -> IO Wrapper.Value
codegenTerm (TermId name) builder m = 
    do
        error "not implemented: Term id"

codegenTerm (TermExpr expr) builder m = 
    do
        val <- codegenExpr expr builder m
        return val

codegenTerm (TermNum num) builder m =
    do
        return (Wrapper.constInt Wrapper.int64Type (fromIntegral num) (False :: Bool))

codegenTerm (TermCall name params) builder m =
    do
        fun <-  (Wrapper.getNamedFunction m name) 
        calleeFun <- case fun of 
            Nothing -> error "function not found"
            Just fun -> return fun

        call <- Wrapper.buildCall builder calleeFun [] "calltmp"
        return call




