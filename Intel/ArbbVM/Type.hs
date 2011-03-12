{- Things that have to do with ArBB types -} 


module Intel.ArbbVM.Type (size) where
 
import Intel.ArbbVM 


-- size of one element of ArBB scalartype in bytes
size :: ScalarType -> Int
size ArbbI8  = 1
size ArbbI16 = 2 
size ArbbI32 = 4
size ArbbI64 = 8 
size ArbbU8  = 1 
size ArbbU16 = 2 
size ArbbU32 = 4 
size ArbbU64 = 8
size ArbbF32 = 4
size ArbbF64 = 8 
size ArbbBoolean = 4 -- I think they are stored as 32 bit integers
size ArbbUsize = 64  -- very insecure about the three last here
size ArbbIsize = 64  -- What size are these really ? 
