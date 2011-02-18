{-# LANGUAGE CPP, ScopedTypeVariables #-}

import Intel.ArbbVM 
import Intel.ArbbVM.Convenience

import Foreign.Marshal.Array
import Foreign.Ptr 
import Foreign.ForeignPtr

import C2HS


main = do 
     ctx   <- getDefaultContext 
     sty   <- getScalarType ctx ArbbI32
     fnt   <- getFunctionType ctx [sty] [sty] 
     one   <- newConstant ctx sty (1::Int32)
     ten   <- newConstant ctx sty (10::Int32)

-- This is not allowed:
--     dummy <- getFunctionType ctx [] [] 

     ------------------------------------------------------------
     myfun <- beginFunction ctx fnt "foo" 0
     inp   <- getParameter myfun 0 0 
     out   <- getParameter myfun 1 0

     ------------------------------
     -- This is not a nested function because it doesn't have a link
     -- to the parent, but it is an "interleaved" function in the VM stream.
     fun2 <- beginFunction ctx fnt "bar" 0
     a    <- getParameter fun2 0 0 
     b    <- getParameter fun2  1 0
     op fun2 ArbbOpCopy [b] [a]
     op fun2 ArbbOpAdd  [b] [b,one]
     endFunction fun2
     ------------------------------

     op myfun ArbbOpCopy [out] [inp]
     callOp myfun ArbbOpCall fun2 [out] [inp]

     op myfun ArbbOpAdd  [out] [out,ten]
     endFunction myfun
     ------------------------------------------------------------
     putStrLn "Done streaming function AST."
     compile myfun
     putStrLn "Done compiling function, now executing..."

     x <- newConstant ctx sty (100::Int32)

     binding <- getBindingNull 
     g <- createGlobal ctx sty "res" binding
     y <- variableFromGlobal ctx g     
     execute myfun [y] [x]

     result <- readScalarOfSize 4 ctx y :: IO Int32
     putStrLn$ "Result from function application: "++ show result
