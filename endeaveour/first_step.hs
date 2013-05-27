import Data.Int
import LLVM.Core

mIncr :: CodeGenModule (Function (Int32 -> IO Int32))
mIncr = 
  createNamedFunction ExternalLinkage "incr" $ \ x -> do
    r <- add x (1 :: Int32)
    ret r

main = do
    m <- newModule
    defineModule m mIncr
    writeBitcodeToFile "incr.bc" m
