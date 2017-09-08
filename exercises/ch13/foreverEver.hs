import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower, isLetter)


isIt :: String -> Bool
isIt s = clean == reverse clean
  where clean = filter isLetter s


palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case isIt line1 of
    True -> do
      putStrLn "It's a palindrome"
      exitSuccess
    False -> putStrLn "sorry but no"
