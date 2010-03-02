
import StupidGenerated
import System.IO
import Data.IORef

main = do
  stdoutRef <- newIORef stdout
  test stdoutRef

