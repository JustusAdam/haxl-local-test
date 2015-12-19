module Main where

import Lib
import Haxl.Core
import Data.Traversable
import Data.Maybe


singleton a = [a]

main :: IO ()
main = do
  myEnv <- initEnv (stateSet StoreReqState stateEmpty) StoreReqState
  data' <- runHaxl myEnv $
    (++)
      <$> sequenceA
        [ getActor "Harry Potter"
        , getActor "Ron Weasley"
        , getActor "Argus Filtch" >>= maybe (getActor "Minerva McGonagall") (return . Just)
        ]
      <*> do
        h <- getActor "Harry Potter"
        let ron = getActor "Ron Weasley"
        case h of
          Nothing -> singleton <$> ron
          Just _ -> (: [h]) <$> ron

  print $ catMaybes data'
