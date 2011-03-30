
module Sum where 

import Data.Array.Accelerate as Acc

sumAcc :: Vector Float -> Acc (Scalar Float)
sumAcc xs 
  = let
      xs' = use xs
    in
    Acc.fold (+) 0 xs' 
