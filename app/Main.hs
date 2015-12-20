module Main where

import DataSources.Actors
import DataSources.Movies
import Haxl.Core
import Data.Traversable
import Data.Maybe


singleton a = [a]

main :: IO ()
main = do
  myEnv <- initEnv
             (stateSet MovieReqState $
               stateSet StoreReqState stateEmpty)
             StoreReqState
  data' <- runHaxl myEnv $
    (++)
      <$> sequenceA -- All top-level fetches here will be batched in the first
                    -- round because sequenceA uses Applicative
        [ getActor "Harry Potter"
        , getActor "Ron Weasley"
        , getActorForMovie "Inception"
        , getActor "Argus Filtch" >>=
            maybe -- `maybe` does a pattern match to create a data dependency and
                  -- push the subsequent request to the second round
              (getActor "Minerva McGonagall")
              (return . Just)
        ]
      <*> do
        h <- getActor "Harry Potter" -- this will also land in the first round
                                     -- because it is the topmost fetch in the
                                     -- do block
        let ron = getActor "Ron Weasley"
        case h of -- and a pattern match again to push `ron` to the second round
          Nothing -> singleton <$> ron
          Just _ -> (: [h]) <$> ron

  print $ catMaybes data'
