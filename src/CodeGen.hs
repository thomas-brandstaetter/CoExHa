
    module CodeGen where


import LLVM.Core
import TypeNames
import Data.Int (Int64)
import Data.Word (Word8, Word32)



codegenExpr :: [(String, Value (Ptr Int64))] -> Expression -> CodeGenFunction r (Value Int64)
codegenExpr ls (Identifier s) = case lookup s ls of
    Nothing -> error $ "unknown identifier: " ++ s
    (Just v) -> load v
codegenExpr _ (Number n) = return $ valueOf $ fromIntegral n
codegenExpr ls (Plus e1 e2)     = arith ls e1 e2 iadd
codegenExpr ls (Minus e1 e2)    = arith ls e1 e2 isub
codegenExpr ls (Multiply e1 e2) = arith ls e1 e2 imul
codegenExpr ls (Divide e1 e2)   = arith ls e1 e2 idiv

arith ls e1 e2 f = do
    lhs <- codegenExpr ls e1
    rhs <- codegenExpr ls e2
    f lhs rhs

codegenCond :: [(String, Value (Ptr Int64))] -> Condition -> CodeGenFunction r (Value Bool)
codegenCond ls (Eq e1 e2) = cnd ls e1 e2 CmpEQ
codegenCond ls (Ne e1 e2) = cnd ls e1 e2 CmpNE
codegenCond ls (Gt e1 e2) = cnd ls e1 e2 CmpGT
codegenCond ls (Lt e1 e2) = cnd ls e1 e2 CmpLT
codegenCond ls (Ge e1 e2) = cnd ls e1 e2 CmpGE
codegenCond ls (Le e1 e2) = cnd ls e1 e2 CmpLE

cnd ls e1 e2 f = do
    lhs <- codegenExpr ls e1
    rhs <- codegenExpr ls e2
    cmp f lhs rhs

codegenStatement :: [(String, Value (Ptr Int64))] -> Statement -> CodeGenFunction () ()
codegenStatement ls (Assign id e) = case lookup id ls of
    Nothing -> error $ "unknown identifier: " ++ id
    (Just v) -> do
        val <- codegenExpr ls e
        store val v
codegenStatement ls (Begin stmts) = mapM_ (codegenStatement ls) stmts
codegenStatement ls (If cond s1) = do
    ifbl <- newBasicBlock
    thenbl <- newBasicBlock

    cnd <- codegenCond ls cond
    condBr cnd ifbl thenbl

    defineBasicBlock ifbl
    codegenStatement ls s1
    ret ()

    defineBasicBlock thenbl
    ret ()
codegenStatement ls (While cond s) = do
    exit <- newBasicBlock
    while <- newBasicBlock

    defineBasicBlock while
    cnd <- codegenCond ls cond
    codegenStatement ls s
    condBr cnd while exit

    defineBasicBlock exit
    ret ()

codegenBlock :: [(String, Value (Ptr Int64))] -> Block -> CodeGenModule (Function (IO ()))
codegenBlock vls (Block _ vars _ stmt) = do
    -- And here is the type error
    func <- createFunction ExternalLinkage $ do
        ls <- mapM named vars
        codegenStatement (vls ++ ls) stmt
        mapM_ (free . snd) ls
    return func
    where
        named n = do
            v <- alloca
            return (n, v)


codegen block file = do

    m <- newModule
    defineModule m $ codegenBlock [] block
    writeBitcodeToFile file m

    print "codegen done"
