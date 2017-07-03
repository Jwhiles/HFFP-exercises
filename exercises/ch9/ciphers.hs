module Cipher where

import Data.Char

cipher :: Int -> Char -> Char
cipher n c
  | isUpper c = chr $ (mod ((ord c) - 64 + n) 26) + 64
  | otherwise = chr $ (mod ((ord c) - 96 + n) 26) + 96

cipherMessage :: Int -> String -> String
cipherMessage _ [] = []
cipherMessage n (x:xs) = cipher n x : cipherMessage n xs

unCeaser :: Int -> String -> String
unCeaser n s = cipherMessage (-n) s
