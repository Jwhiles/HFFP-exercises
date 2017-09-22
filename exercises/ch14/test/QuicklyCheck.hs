module QuicklyCheck where

import Test.QuickCheck
import Test.Hspec

half x = x / 2

halfIdentity = (*2) . half


propertyTest :: IO ()
propertyTest = hspec $ do
  describe "a property test" $ do
    it "half x * 2 == x" $ do
      property $ \x -> halfIdentity x == (x :: Float) 
