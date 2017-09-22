module QuicklyCheck where

import Test.QuickCheck
import Test.Hspec
import Data.Char (toUpper)
half x = x / 2

halfIdentity = (*2) . half


propertyTest :: IO ()
propertyTest = hspec $ do
  describe "a property test" $ do
    it "half x * 2 == x" $ do
      property $ \x -> halfIdentity x == (x :: Float) 

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord s = (toUpper $ head s):(tail s)

idempot :: IO ()
idempot = hspec $ do
 describe "capitalise word is idempotent" $ do
   it "keep on capitalising" $ do
     property $ \x -> twice capitalizeWord x == fourTimes capitalizeWord x
