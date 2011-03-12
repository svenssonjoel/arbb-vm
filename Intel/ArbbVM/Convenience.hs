{-# LANGUAGE CPP #-}

{- |  

   A module capturing common patterns in ArBB VM code emission and
   thereby making it easier to emit code.

 -}

module Intel.ArbbVM.Convenience 
 (
   ifThenElse, while, readScalarOfSize, newConstant,

   arbbSession, EmitArbb,  
   if_, while_, readScalar_,
   funDef_, funDefS_,
   op_, opImm_,  
   opDynamic_, opDynamicImm_,
   map_,  call_, 

   mapToHost_,

   const_, int32_, int64_,float32_, float64_, bool_,
   const_storable_,
   usize_, isize_, 
   incr_int32_, copy_,

   local_bool_, local_int32_, local_float64_, 
   global_nobind_, global_nobind_int32_,

   compile_, execute_, serializeFunction_, finish_, 

   getBindingNull_, getScalarType_, variableFromGlobal_,
   getFunctionType_, createGlobal_, createLocal_,

   createDenseBinding_,  getDenseType_,
   getNestedType_, 

   withArray_, print_,

   doarith_, SimpleArith(V),
--   module Intel.ArbbVM.SimpleArith,

   liftIO, liftMs,

   -- These should probably be internal only:
   getCtx, getFun
 )
where

--import qualified Intel.ArbbVM as VM
import Intel.ArbbVM as VM
-- import Intel.ArbbVM.SimpleArith 

import Control.Monad
import Data.IORef
import Data.Word
import Data.Int
import Data.Serialize
import Data.ByteString.Internal
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.ForeignPtr
import Foreign.Storable as Storable
import Foreign.Ptr 
import C2HS

import qualified  Control.Monad.State.Strict as S 

import Debug.Trace

--------------------------------------------------------------------------------
-- The monad for emitting Arbb code.  
type EmitArbb = S.StateT ArbbEmissionState IO

-- We put the context and a stack of function types into the
-- background.  Note, we need the stack of functions because we allow
-- nested function definitions at this convenience layer.  (They will
-- all have global scope to ArBB however.)
type ArbbEmissionState = (Context, [Function])
-- Note: if we also include a counter in here we can do gensyms...


#define L S.lift$

liftIO :: IO a -> EmitArbb a
liftIO = S.lift -- Allow the user to perform IO inside this monad.

arbbSession :: EmitArbb a -> IO a 
arbbSession m = 
  do ctx <- getDefaultContext
     (a,s) <- S.runStateT m (ctx,[])
     return a

getFun msg = 
 do (_,ls) <- S.get
    case ls of 
      [] -> error$ msg ++" when not inside a function"
      (h:t) -> return h

getCtx = 
 do (ctx,_) <- S.get
    return ctx

------------------------------------------------------------------------------
-- Map an ArBB array into host addrspace
mapToHost_ :: Variable -> [Word64] -> RangeAccessMode -> EmitArbb (Ptr ())
mapToHost_ var pitch mode = do 
   ctx <- getCtx
   L mapToHost ctx var pitch mode 
--------------------------------------------------------------------------------
-- Convenience functions for common patterns:

opImm_ :: Opcode -> [Variable] -> [Variable] -> EmitArbb()
opImm_ code out inp = L opImm code out inp

op_ :: Opcode -> [Variable] -> [Variable] -> EmitArbb ()
op_ code out inp = 
 do fun <- getFun "Convenience.op_ cannot execute an Opcode"
    L op fun code out inp

opDynamicImm_ :: Opcode -> [Variable] -> [Variable] -> EmitArbb()
opDynamicImm_ code out inp = L opDynamicImm code out inp

opDynamic_ :: Opcode -> [Variable] -> [Variable] -> EmitArbb ()
opDynamic_ code out inp = 
  do fun <- getFun "Convenience.opDynamic_ cannot execute an Opcode"
     L opDynamic fun code out inp

if_ :: Variable -> EmitArbb a -> EmitArbb a1 -> EmitArbb ()
if_ c t e =
  do fun <- getFun "Convenience.if_ cannot execute a conditional"
     L ifBranch fun c 
     t -- op myfun ArbbOpSub [c] [a,a]
     L elseBranch fun 
     e -- op myfun ArbbOpDiv [c] [a,a]
     L endIf fun

-- | An ArBB while loop.  Must be called inside a function definition.
while_ :: (EmitArbb Variable) -> EmitArbb a -> EmitArbb a
while_ cond body = 
   do fun <- getFun "Convenience.while_ cannot execute a while loop"
      L beginLoop fun ArbbLoopWhile
      L beginLoopBlock fun ArbbLoopBlockCond
      lc <- cond
      L loopCondition fun lc 
      L beginLoopBlock fun ArbbLoopBlockBody
      result <- body 
      L endLoop fun
      return result


const_storable_ :: Storable a => ScalarType -> a -> EmitArbb Variable 
const_storable_ st n = 
  do ctx <- getCtx
     L newConstantAlt ctx st n

-- This version picks the right in-memory representation.
const_ :: Integral a => ScalarType -> a -> EmitArbb Variable 
const_ sty i =
   case sty of 
     ArbbI8  -> const_storable_ sty (fromIntegral i :: Int8)
     ArbbI16 -> const_storable_ sty (fromIntegral i :: Int16)
     ArbbI32 -> const_storable_ sty (fromIntegral i :: Int32)
     ArbbI64 -> const_storable_ sty (fromIntegral i :: Int64)

     ArbbU8  -> const_storable_ sty (fromIntegral i :: Word8)
     ArbbU16 -> const_storable_ sty (fromIntegral i :: Word16)
     ArbbU32 -> const_storable_ sty (fromIntegral i :: Word32)
     ArbbU64 -> const_storable_ sty (fromIntegral i :: Word64)

     -- This only lets you get at the integral floating point numbers:
     ArbbF32 -> const_storable_ sty (fromIntegral i :: Float)
     ArbbF64 -> const_storable_ sty (fromIntegral i :: Double)

     ArbbUsize -> const_storable_ sty (fromIntegral i :: Word)
     ArbbIsize -> const_storable_ sty (fromIntegral i :: Int)

 

readScalar_ :: (Num a, Storable a) =>  Variable -> EmitArbb a
readScalar_ v = 
  do ctx <- getCtx
     let z = 0
	 size = Storable.sizeOf z
     x <- L readScalarOfSize size ctx v 
     return (x+z)

type FunBody = [Variable] -> [Variable] -> EmitArbb ()

debug_fundef = True

funDef_ :: String -> [Type] -> [Type] -> FunBody  -> EmitArbb Function
funDef_ name outty inty userbody = 
  do 
     ctx <- getCtx
     fnt     <- L getFunctionType ctx outty inty
     fun     <- L beginFunction ctx fnt name 0

     when debug_fundef$ print_$ "["++name++"] Function begun."
     invars  <- L forM [0 .. length inty  - 1]   (getParameter fun 0)
     outvars <- L forM [0 .. length outty - 1]   (getParameter fun 1)

     -- Push on the stack:
     S.modify (\ (c,ls) -> (c, fun:ls))

     -- Now generate body:
     when debug_fundef$ print_$ "["++name++"]  Begin body codgen..."
     userbody outvars invars
     when debug_fundef$ print_$ "["++name++"]  Done body codgen."

     -- Pop off the stack:
     S.modify (\ (c, h:t) -> (c, t))
     L endFunction fun

     -- EXPERIMENTAL!  Compile immediately!!
     when debug_fundef$ print_$ "["++name++"] Function ended. Compiling..."
     L compile fun
     when debug_fundef$ print_$ "["++name++"] Done compiling."
     return fun

-- Umm... what's a good naming convention here?
funDefS_ :: String -> [ScalarType] -> [ScalarType] -> FunBody  -> EmitArbb Function
funDefS_ name outs ins body =
  do 
     outs' <- mapM getScalarType_ outs
     ins'  <- mapM getScalarType_ ins
     funDef_ name outs' ins' body
  

call_ :: Function -> [Variable] -> [Variable] -> EmitArbb ()
call_ fun out inp = 
  do -- At the point of the call the *caller* is on the top of the stack:
     caller <- getFun "Convenience.call_ cannot call function"
     when debug_fundef$ print_ "Call_: got caller function, emitting call opcode..."
     L callOp caller ArbbOpCall fun out inp
     when debug_fundef$ print_ "Call_: Done emitting call opcode."

map_ :: Function -> [Variable] -> [Variable] -> EmitArbb ()
map_ fun out inp = 
  do -- At the point of the call the *caller* is on the top of the stack:
     caller <- getFun "Convenience.map_ cannot call function"
     when debug_fundef$ print_ "Map_: got caller function, emitting map opcode..."
     L callOp caller ArbbOpMap fun out inp
     when debug_fundef$ print_ "Map_: Done emitting map opcode."

--------------------------------------------------------------------------------
-- Iteration Patterns.

-- for_range_ :: Variable -> Variable -> (i -> EmitArbb ()) -> EmitArbb ()

-- This uses C-style [inclusive,exclusive) ranges.
-- for_constRange_ :: Int -> Int -> (i -> EmitArbb ()) -> EmitArbb ()
-- for_range_ start end body = do 
--    counter <- local_int32_ "counter"
--    op_ ArbbOpCopy [counter] [zer]
--    while_ (do
--        lc <- local_bool_ "loopcond"
--        op_ ArbbOpLess [lc] [counter,max]
--        return lc)
--     (op_ ArbbOpAdd [counter] [counter,one])

-- Lifting higher order ops like this is trickier.
withArray_ :: Storable a => [a] -> (Ptr a -> EmitArbb b) -> EmitArbb b
withArray_ ls body = 
 do state <- S.get
    ref   <- L newIORef state
    let body2 ptr = do (a,s2) <- S.runStateT (body ptr) state
     		       writeIORef ref s2
     		       return a
    res    <- L withArray ls body2
    state2 <- L readIORef ref
    S.put state2
    return res

print_ :: String -> EmitArbb ()
print_ = S.lift . putStrLn

--------------------------------------------------------------------------------

-- liftM for lists
liftMs :: Monad m => ([a] -> m b) -> [m a] -> m b
-- liftMs fn ls = liftM fn (sequence ls)
liftMs fn ls = 
  sequence ls >>= fn
  -- do ls' <- sequence ls 
  --    fn ls'

-- These let us lift the slew of ArbbVM functions that expect a Context as a first argument.
lift1 :: (Context -> a -> IO b)                -> a                -> EmitArbb b
lift2 :: (Context -> a -> b -> IO c)           -> a -> b           -> EmitArbb c
lift3 :: (Context -> a -> b -> c -> IO d)      -> a -> b -> c      -> EmitArbb d
lift4 :: (Context -> a -> b -> c -> d -> IO e) -> a -> b -> c -> d -> EmitArbb e

lift1 fn a       = do ctx <- getCtx; L fn ctx a 
lift2 fn a b     = do ctx <- getCtx; L fn ctx a b
lift3 fn a b c   = do ctx <- getCtx; L fn ctx a b c
lift4 fn a b c d = do ctx <- getCtx; L fn ctx a b c d

getScalarType_      = lift1 getScalarType
getDenseType_       = lift2 getDenseType  
getNestedType_      = lift1 getNestedType
variableFromGlobal_ = lift1 variableFromGlobal
getFunctionType_    = lift2 getFunctionType
createGlobal_       = lift3 createGlobal

createLocal_ :: Type -> String -> EmitArbb Variable
createLocal_ ty name = do f <- getFun "Convenience.createLocal_ cannot create local"
			  L createLocal f ty name

createDenseBinding_ = lift4 createDenseBinding

----------------------------------------
-- These are easy ones, no Context or Function argument:

compile_ fn      = liftIO$ compile fn
execute_ a b c   = liftIO$ execute a b c
finish_          = liftIO finish
serializeFunction_ = liftIO . serializeFunction
getBindingNull_  = liftIO getBindingNull



-- ... TODO ...  Keep going.

--------------------------------------------------------------------------------

-- Lazy, lazy, lazy: here are even more shorthands.

int32_   :: Integral t => t -> EmitArbb Variable 
int64_   :: Integral t => t -> EmitArbb Variable 
usize_   :: Integral t => t -> EmitArbb Variable
isize_   :: Integral t => t -> EmitArbb Variable
float32_ :: Float           -> EmitArbb Variable 
float64_ :: Double          -> EmitArbb Variable 

int32_   = const_ ArbbI32 
int64_   = const_ ArbbI64 
usize_   = const_ ArbbUsize
isize_   = const_ ArbbIsize 
float32_ = const_storable_ ArbbF32 
float64_ = const_storable_ ArbbF64 



bool_ :: Bool -> EmitArbb Variable
bool_ True  = const_storable_ ArbbBoolean (1::Int32)
bool_ False = const_storable_ ArbbBoolean (0::Int32)

-- TODO... Keep going...

incr_int32_ :: Variable -> EmitArbb ()
incr_int32_ var = do one <- int32_ 1
		     op_ ArbbOpAdd [var] [var,one] 

copy_ v1 v2 = op_ ArbbOpCopy [v1] [v2]


------------------------------------------------------------

local_bool_ name = do bty <- getScalarType_ ArbbBoolean
		      createLocal_ bty name

local_int32_ name = do ity <- getScalarType_ ArbbI32
		       createLocal_ ity name

local_float64_ name = do ty <- getScalarType_ ArbbF64
		         createLocal_ ty name

global_nobind_ ty name = 
  do binding <- getBindingNull_
     g       <- createGlobal_ ty name binding
     variableFromGlobal_ g

global_nobind_int32_ name = 
  do sty <- getScalarType_ ArbbI32
     global_nobind_ sty name

--------------------------------------------------------------------------------
-- Num instance.




--------------------------------------------------------------------------------
-- OBSOLETE: These were some helpers that didn't use the EmitArbb monad.

ifThenElse :: Function -> Variable -> IO a -> IO a1 -> IO ()
ifThenElse f c t e =
  do
   ifBranch f c      
   t -- op myfun ArbbOpSub [c] [a,a]
   elseBranch f 
   e -- op myfun ArbbOpDiv [c] [a,a]
   endIf f


-- while loops
while :: Function -> (IO Variable) -> IO a1 -> IO ()
while f cond body = 
   do 
     beginLoop f ArbbLoopWhile
     beginLoopBlock f ArbbLoopBlockCond
     lc <- cond
     loopCondition f lc 

     beginLoopBlock f ArbbLoopBlockBody
     body 
     endLoop f     

-- Works not just for arrays but anything serializable:
withSerialized :: Serialize a => a -> (Ptr () -> IO b) -> IO b
withSerialized x fn =    
   withForeignPtr fptr (fn . castPtr)
 where 
   (fptr,_,_) = toForeignPtr (encode x)

newConstant :: Storable a => Context -> Type -> a -> IO Variable 
newConstant ctx t n = 
  do           
   -- Could use withSerialized possibly...
   tmp <- withArray [n] $ \x -> createConstant ctx t (castPtr x)
   variableFromGlobal ctx tmp

newConstantAlt :: Storable a => Context -> ScalarType -> a -> IO Variable 
newConstantAlt ctx st n = 
  do           
   t   <- getScalarType ctx st       
   tmp <- withArray [n] $ \x -> createConstant ctx t (castPtr x)
   variableFromGlobal ctx tmp

-- global/constant shortcuts

-- readScalarOfSize :: Storable b => Int -> Context -> Variable -> EmitArbb b
readScalarOfSize :: Storable b => Int -> Context -> Variable -> IO b
readScalarOfSize n ctx v = 
    allocaBytes n $ \ptr -> 
       do       
        readScalar ctx v ptr 
        peek (castPtr ptr)

-- TODO: readScalar of storable should be able to determine size.


----------------------------------------------------------------------------------------------------
-- Numeric instances.
----------------------------------------------------------------------------------------------------

{- 
  It is not clear that this is worth it, yet.
  This is mainly here because I want to use Data.Complex.
 -}


-- We could use a richer type for Variable in the convenience interface.
-- data VarPlus = VarPlus Variable Type 
-- 
-- The trick would be to have functions like op_ take [SimpleArith] rather than [Variable]
-- But to dealwith anything other than the "V" variant, the desired type would need to be known.
-- 

instance Show Variable where
  show v = "<ArBB_Var>"

instance Eq Variable where 
  a == b = error "equality on Variables doesn't make sense yet"

data SimpleArith = 
		   V      Variable
		 | Const  Integer
		 | ConstD Double

                 | Plus   SimpleArith SimpleArith
		 | Times  SimpleArith SimpleArith
		 | Div    SimpleArith SimpleArith
		 | Signum SimpleArith
		 | Abs    SimpleArith

		 | Expon SimpleArith
		 | Sqrt  SimpleArith
		 | Log   SimpleArith
		 | Sin   SimpleArith
		 | Cos   SimpleArith
		 | ASin  SimpleArith
		 | ACos  SimpleArith
		 | ATan  SimpleArith
		 | SinH  SimpleArith
		 | CosH  SimpleArith
		 | ASinH SimpleArith
		 | ACosH SimpleArith
		 | ATanH SimpleArith

--		 | ProperFrac SimpleArith
  -- UNFINISHED
  deriving (Show,Eq)


instance Num SimpleArith where  
  (+)         = Plus
  (*)         = Times
  signum      = Signum
  abs         = Abs
  fromInteger = Const 

instance Fractional SimpleArith where
  (/)              = Div
  fromRational rat = error "fromRational not implemented yet for SimpleArith"

instance Ord SimpleArith where 
  a < b = error "< not implemented yet for SimpleArith"

instance Real SimpleArith where 
  toRational v = error "toRational not implemented for SimpleArith"

instance Floating SimpleArith where
  pi   = ConstD pi
  exp  = Expon
  sqrt = Sqrt
  log  = Log
  -- (**) :: a -> a -> a
  -- logBase :: a -> a -> a
  sin  = Sin
  -- tan :: a -> a
  cos  = Cos
  asin = ASin
  atan = ATan
  acos = ACos
  sinh = SinH
  cosh = CosH
  asinh = ASinH
  acosh = ACosH
  atanh = ATanH

-- instance Enum SimpleArith where
-- instance Integral SimpleArith where

--class (Real a, Fractional a) => RealFrac a where
instance RealFrac SimpleArith where
--  properFraction :: Integral b => a -> (b, a)
  properFraction x = error "properFraction not implemented for SimpleArith"

#if 0

class (Real a, Enum a) => Integral a where
  quot :: a -> a -> a
  rem :: a -> a -> a
  div :: a -> a -> a
  mod :: a -> a -> a
  quotRem :: a -> a -> (a, a)
  divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer

class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]

class (RealFrac a, Floating a) => RealFloat a where
  floatRadix :: a -> Integer
  floatDigits :: a -> Int
  floatRange :: a -> (Int, Int)
  decodeFloat :: a -> (Integer, Int)
  encodeFloat :: Integer -> Int -> a
  exponent :: a -> Int
  significand :: a -> a
  scaleFloat :: Int -> a -> a
  isNaN :: a -> Bool
  isInfinite :: a -> Bool
  isDenormalized :: a -> Bool
  isNegativeZero :: a -> Bool
  isIEEE :: a -> Bool
  atan2 :: a -> a -> a

#endif


-- | This lets one execute simple arithmetic expressions and store the result.
--   Returns the name of a new local binding that caries the result.
doarith_ :: ScalarType -> SimpleArith -> EmitArbb Variable
doarith_ ty_ exp = 
   do ty <- getScalarType_ ty_
      let binop op a b = 
	      do tmp <- createLocal_ ty "tmp"
		 a'  <- loop a
		 b'  <- loop b
		 op_ op [tmp] [a',b']
		 return tmp
	  loop exp = 
	     case exp of 
	       V     v   -> return v
	       Const i   -> const_ ty_ i

	       Plus  a b -> binop ArbbOpAdd a b
	       Times a b -> binop ArbbOpMul a b
	       _ -> error$ "doarith_: not handled yet: "++ show exp
      loop exp


-- data ScalarType = ArbbI8
--                 | ArbbI16
--                 | ArbbI32
--                 | ArbbI64

--                 | ArbbU8
--                 | ArbbU16
--                 | ArbbU32
--                 | ArbbU64

--                 deriving (Enum,Show,Eq)
