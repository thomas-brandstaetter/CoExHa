import LLVM.Core as Core
import LLVM.FFI.Core as FFI
import LLVM.Wrapper.Core as Wrapper
import LLVM.Wrapper.BitWriter as BitWriter





main = do

    m <-  Wrapper.moduleCreateWithName "__global_module__"

    
    let methodType = Wrapper.functionType FFI.int64Type [] (False :: Bool)
    method <- Wrapper.addFunction m "function" methodType


    bld <- Wrapper.createBuilder
    entry <- Wrapper.appendBasicBlock method "entry"

    Wrapper.buildAdd 
                bld 
                (constInt int64Type 150 (False::Bool))
                (constInt int64Type 50 (False::Bool))
                "super"

    
    Wrapper.positionAtEnd bld entry




 
    BitWriter.writeBitcodeToFile m "bitcode.bc"


    putStrLn "Done"
