
module Incr where 

import Data.Array.Accelerate as Acc 

incrAcc :: Vector Float -> Acc (Vector Float)
incrAcc xs
  = let
      xs' = use xs
    in
    Acc.map (\x -> constant 1.0 + x) xs'

