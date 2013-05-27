import LLVM.Core
import LLVM.ExecutionEngine
import Data.Int



mAddMul :: CodeGenModule (Function (Int32 -> Int32 -> Int32 -> IO Int32))
mAddMul = 
  createFunction ExternalLinkage $ \ x y z -> do
    t <- add x y
    r <- mul t z
    ret r



main = do
    addMul <- simpleFunction mAddMul
    a <- addMul 2 3 4
    print a
