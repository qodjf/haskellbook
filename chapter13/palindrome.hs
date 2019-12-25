import Control.Monad
import Data.Char
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  (let line2 = map toLower . filter isLetter $ line1 in
    case (line2 == reverse line2) of
      True -> putStrLn "It's a palindrome!"
      False -> do
        putStrLn "Nope!"
        exitSuccess)
