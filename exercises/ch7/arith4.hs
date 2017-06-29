module Arith4 where

roundTrip :: (Show a, Read b) => a -> b
roundTrip a = read (show a)

pointFreeTrip :: (Show a, Read a) => a -> a
pointFreeTrip = read . show

main = do
  print ((roundTrip 4) :: Int)
  -- parens are needed the type of b concrete
  print (pointFreeTrip 4)
  print (id 4)
