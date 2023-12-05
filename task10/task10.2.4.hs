import Data.IORef (IORef, readIORef, newIORef, modifyIORef')
import Control.Monad (when)
while :: IORef a -> (a -> Bool) -> IO () -> IO ()
while ref p action = do
    val <- readIORef ref
    when (p val) $ do
        action
        while ref p action

factorial :: Integer -> IO Integer
factorial n = do
  r <- newIORef 1
  i <- newIORef 1
  while i (<= n) ( do
    ival <- readIORef i
    modifyIORef' r (* ival)
    modifyIORef' i (+ 1)
   )
  readIORef r