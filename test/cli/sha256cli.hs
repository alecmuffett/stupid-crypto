
module Main where 
import StupidGenerated hiding (main)
import StupidStuff
import Data.Char
import Control.Monad.ST
import Data.STRef
import Numeric
import System.IO

main = do
  hPutStrLn stderr "SHA256 Stupid-Haskell commandline tool"
  interact sha256sum

-- stretching out the input to have enough space for padding bits
-- should be done inside the sha256 stupid code, not in the calling haskell
-- code, I think.

sha256sum :: String -> String
sha256sum inputString = 
  runST $ do

   -- need to pad length so that message buffer has space for extra
   -- stuff that we'll put on the end

   -- adding 64 bytes is always enough, i think

   let paddedString = inputString ++ (take 64 $ repeat 'X')

   outSTRefs <- sequence $ take 32 $ repeat (newSTRef (0 :: Uint8))
   outArray <- newSTRef outSTRefs
   let message = map charToUInt8 paddedString
   messageSTRefs <- mapM newSTRef message
   messageArray <- newSTRef messageSTRefs
   length <- newSTRef (horribleCastTo32 $ 8 * length inputString)
-- would be nice if we didn't have to pass in the length explicitly
-- if we assume that the byte string is always a multiple of 8 bits long
-- then thats possible in a Haskell structure that knows its own length...
-- if its a non-8-bit aligned structure, maybe we still would have a
-- (stupid-specific?) structure for representing arbitrary list of bits?
   sha256 outArray messageArray length
   outVs <- mapM readSTRef outSTRefs
   return $ foldr1 (++) (map intToHex outVs)
 
-- sha256 has this signature:
-- sha256 :: STRef s  [STRef s Uint8]  -> STRef s  [STRef s Uint8]  -> STRef s Uint32 ->  ST s ()
 
charToUInt8 :: Char -> Uint8
charToUInt8 c = horribleCastTo8 (ord c)


-- courtesty of http://stackoverflow.com/questions/1959715/how-to-print-integer-literals-in-binary-or-hex-in-haskell

intToHex i = showIntAtBase 16 intToDigit i ""

