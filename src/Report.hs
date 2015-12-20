module Report (logLine, logIt) where

import System.IO


logLine :: String -> IO ()
logLine = hPutStrLn stderr


logIt :: Show a => a -> IO ()
logIt = logLine . show
