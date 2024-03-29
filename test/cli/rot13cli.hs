
module Main where 
import StupidGenerated hiding (main)
import StupidStuff
import Data.Char
import Control.Monad.ST
import Data.STRef
import Numeric
import System.IO

main = do
  hPutStrLn stderr "ROT13 Stupid-Haskell commandline tool"
  interact rot13string

rot13string :: String -> String
rot13string inputString = map rot13char inputString

rot13char :: Char -> Char
rot13char c = 
  runST $ do
   characterSTRef <- newSTRef (fromIntegral $ ord c) -- maybe an overflow here with utf8
   outputSTRef <- newSTRef 0
   rot13 outputSTRef characterSTRef
   r <- readSTRef outputSTRef
   return $ chr $ fromIntegral r
 
