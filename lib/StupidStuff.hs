module StupidStuff where

  import Data.Bits
  import Data.Char
  import Data.IORef
  import Data.Word
  import Data.STRef

  import Control.Applicative
  import Control.Monad
  import Control.Monad.ST

  import System.IO

-- -- -- stupid data types -- -- --

-- stupid uint32 works mostly like a haskell Word32
-- behaviour of this on overflow is maybe not right - I haven't made
-- any effort to check

  type Uint8 = Word8
  type Uint32 = Word32


-- operators. these work in the monad, rather than being operators on
-- raw values. this allows the translated language to have expressions
-- that use them directly

  mod8 :: (Monad m) => (m Uint8) -> (m Uint8) -> (m Uint8)
  a `mod8` b = do
    lhs <- a
    rhs <- b
    return (lhs `mod` rhs) -- TODO is this the right kind of mod?


  mod32 :: (Monad m) => (m Uint32) -> (m Uint32) -> (m Uint32)
  a `mod32` b = do
    lhs <- a
    rhs <- b
    return (lhs `mod` rhs) -- TODO is this the right kind of mod?

  minus8 :: (Monad m) => (m Uint8) -> (m Uint8) -> (m Uint8)
  a `minus8` b = do
    lhs <- a
    rhs <- b
    return (lhs - rhs)


  minus32 :: (Monad m) => (m Uint32) -> (m Uint32) -> (m Uint32)
  a `minus32` b = do
    lhs <- a
    rhs <- b
    return (lhs - rhs)

  plus8 :: (Monad m) => (m Uint8) -> (m Uint8) -> (m Uint8)
  a `plus8` b = do
    lhs <- a
    rhs <- b
    return (lhs + rhs)
  
  plus32 :: (Monad m) => (m Uint32) -> (m Uint32) -> (m Uint32)
  a `plus32` b = do
    lhs <- a
    rhs <- b
    return (lhs + rhs)
  
  le8 :: (Monad m) => (m Uint8) -> (m Uint8) -> (m Bool)
  a `le8` b = do
    lhs <- a
    rhs <- b
    return (lhs <= rhs)


  ge8 :: (Monad m) => (m Uint8) -> (m Uint8) -> (m Bool)
  a `ge8` b = do
    lhs <- a
    rhs <- b
    return (lhs >= rhs)


  eq32 :: (Monad m) => (m Uint32) -> (m Uint32) -> (m Bool)
  a `eq32` b = do
    lhs <- a
    rhs <- b
    return (lhs == rhs)

  ne32 :: (Monad m) => (m Uint32) -> (m Uint32) -> (m Bool)
  a `ne32` b = do
    lhs <- a
    rhs <- b
    return (lhs /= rhs)

  -- TODO whats the type of the RHS of rshift32? its a bit count... 
  -- the haskell Data.Bit shift routine needs an Int, not a general
  -- integer type, for its RHS, although the LHS can be anything...
  rshift32 :: (Monad m) => (m Uint32) -> (m Uint32) -> (m Uint32)
  a `rshift32` b = do
    lhs <- a
    rhs <- b
    return (lhs `shift` (horribleCastToInt (-rhs)))

  lshift32 :: (Monad m) => (m Uint32) -> (m Uint32) -> (m Uint32)
  a `lshift32` b = do
    lhs <- a
    rhs <- b
    return (lhs `shift` (horribleCastToInt (rhs)))

  not32 :: (Monad m) => (m Uint32) -> (m Uint32)
  not32 a = do
    op <- a
    return (complement op)



  -- here the RHS of lshift8 is allowed to be any integer type...
  -- now is that allowed? this stuff is supposed to be very strictly
  -- typed...
  lshift8 :: (Monad m, Integral i) => (m Uint8) -> (m i) -> (m Uint8)
  a `lshift8` b = do
    lhs <- a
    rhs <- b
    return (lhs `shift` (horribleCastToInt rhs))
  
  or8 :: (Monad m) => (m Uint8) -> (m Uint8) -> (m Uint8)
  a `or8` b = do
    lhs <- a
    rhs <- b
    return (lhs .|. rhs)

  or32 :: (Monad m) => (m Uint32) -> (m Uint32) -> (m Uint32)
  a `or32` b = do
    lhs <- a
    rhs <- b
    return (lhs .|. rhs)

  xor32 :: (Monad m) => (m Uint32) -> (m Uint32) -> (m Uint32)
  a `xor32` b = do
    lhs <- a
    rhs <- b
    return (lhs `xor` rhs)

  and32 :: (Monad m) => (m Uint32) -> (m Uint32) -> (m Uint32)
  a `and32` b = do
    lhs <- a
    rhs <- b
    return (lhs .&. rhs)

  and8 :: (Monad m) => (m Uint8) -> (m Uint8) -> (m Uint8)
  a `and8` b = do
    lhs <- a
    rhs <- b
    return (lhs .&. rhs)

  not8 :: (Monad m) => (m Uint8) -> (m Uint8)
  not8 a = do
    op <- a
    return (complement op)

  band :: (Monad m) => (m Bool) -> (m Bool) -> (m Bool)
  a `band` b = do
    lhs <- a
    rhs <- b
    return (lhs && rhs)

  bor :: (Monad m) => (m Bool) -> (m Bool) -> (m Bool)
  a `bor` b = do
    lhs <- a
    rhs <- b
    return (lhs || rhs)


  -- array (list) access
  (!!!) :: (Integral ix) => (IO [IORef a]) -> (IO ix) -> (IO (IORef a))
  list !!! ix = do
    listV <- list
    ixV <- ix
    if ((horribleCastToInt ixV) >= length listV) then
        error $ "Array out of bounds: array length "++(show $ length listV)++", subscript "++(show ixV)
     else 
       return (listV !! (horribleCastToInt ixV))

  -- these should perhaps do some range checking - I don't know
  -- what happens if these are given an out-of-range integer, but
  -- stupid should define what should happen, and these should implement
  -- that. TODO.
  horribleCastToInt :: (Integral n) => n -> Int 
  horribleCastToInt num = fromIntegral num

  horribleCastTo32 :: (Integral n) => n -> Uint32
  horribleCastTo32 num = fromIntegral num

  horribleCastTo8 :: (Integral n) => n -> Uint8
  horribleCastTo8 num = fromIntegral num

  -- horrible in at least 2 ways...
  horribleCastTo8M :: (Monad m, Integral n) => n -> m Uint8
  horribleCastTo8M n = return (horribleCastTo8 n)
  horribleCastTo32M :: (Monad m, Integral n) => n -> m Uint32
  horribleCastTo32M n = return (horribleCastTo32 n)
    

-- TODO the above can be made into base operators over values, and a
-- lifting function

-- applies b to the a, except that a is a monadic action giving the parameter
-- rather than the value itself
  ($<) :: (a -> IO b) -> (IO a) -> (IO b)
  b $< a = do
    intermediate <- a;
    b intermediate

-- this could go in applicative functor syntax too but I'm not trying to
-- do that right now...
  writeIORefInIO :: (IO (IORef t)) -> t -> IO ()
  writeIORefInIO loc val = do
    locV <- loc
    writeIORef locV val

  -- while the first parameter returns true, keep running the second
  -- parameter
  -- probably need tail recursion to stop stack exploding - not sure if
  -- this form is tail recursive in the right way?
  -- stupidwhile :: (Monad m) => (m Bool) -> m () -> m ()
  stupidwhile :: (IO Bool) -> IO () -> IO ()
  stupidwhile condition body = do
    go <- condition
    if go then do
      body
      stupidwhile condition body
     else
      return ()

  writeToOutputStream :: IO Handle -> IO Uint8 -> IO ()
  writeToOutputStream hr vr = do
    h <- hr
    v <- vr
    hPutChar h (chr $ fromIntegral v)
    return ()

  getput = writeToOutputStream

