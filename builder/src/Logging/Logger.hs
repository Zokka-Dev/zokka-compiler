module Logging.Logger
  ( printLog
  , setLogFlag
  )
where
import GHC.IO (unsafePerformIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (when)


shouldLogFlag :: IORef Bool
{-# NOINLINE shouldLogFlag #-}
shouldLogFlag = unsafePerformIO (newIORef False)

setLogFlag :: Bool -> IO ()
setLogFlag = writeIORef shouldLogFlag

printLog :: String -> IO ()
printLog str =
  do
    shouldLog <- readIORef shouldLogFlag
    when shouldLog (print str)
