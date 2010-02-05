
module Main where 
import StupidGenerated hiding (main)
import StupidStuff
import Data.Char
import Control.Monad.ST
import Data.STRef

main = do
  putStrLn "RSA256 Stupid-Haskell commandline tool"
  interact rsa256sum

-- stretching out the input to have enough space for padding bits
-- should be done inside the rsa256 stupid code, not in the calling haskell
-- code, I think.

rsa256sum :: String -> String
rsa256sum inputString = 
  runST $ do

   -- need to pad length so that message buffer has space for extra
   -- stuff that we'll put on the end

   -- adding 64 bytes is always enough, i think

   let paddedString = inputString ++ (take 64 $ repeat 'X')

   outSTRefs <- sequence $ take 8 $ repeat (newSTRef (0 :: Uint32))
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
   return $ show outVs
 
-- sha256 has this signature:
-- sha256 :: STRef s  [STRef s Uint32]  -> STRef s  [STRef s Uint8]  -> STRef s Uint32 ->  ST s ()
 
charToUInt8 :: Char -> Uint8
charToUInt8 c = horribleCastTo8 (ord c)
 
