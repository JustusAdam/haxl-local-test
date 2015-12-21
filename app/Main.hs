{-
I'd love to have written this entire thing in `do` notation, but the version of
ghc on stack does not support the `ApplicativeDo` extension (yet).

As a result using `do` would make a separate round for each dataFetch, which
very much defeats the purpose of the library.
-}
module Main where

import           Data.IORef
import           Data.Maybe
import           Data.Traversable
import           DataSources.Actors
import           DataSources.Movies
import           Haxl.Core


main :: IO ()
main = do
  myEnv <- initEnv
             (stateSet MovieReqState $
               stateSet StoreReqState stateEmpty)
             StoreReqState
  (data', statsRef') <- runHaxl myEnv $ do
    data' <- (++)
        <$> sequenceA -- All top-level fetches here will be batched in the first
                      -- round because sequenceA uses Applicative
          [ getActor "Harry Potter"
          , getActor "Ron Weasley"
          , getActorForMovie "Inception"
          , getActor "Argus Filtch" >>=
            \Nothing -> -- do a pattern match to create a data dependency and
                        -- push the subsequent request to the second round
                        --
                        -- theoretically this is not necessary, because
                        -- ApplicativeDo is not enabled, hence the >>=
                        -- already forces a data dependency, but I wanted to
                        -- make sure this would be stable across compiler versions
              getActor "Minerva McGonagall"
          ]
        <*> (
          getActor "Harry Potter" >>=  -- this will also land in the first round
                                       -- because it is the topmost fetch
            \h@(Just _) -> -- and a pattern match again to push `ron` to the
                           -- second round
              (: [h]) <$> getActor "Ron Weasley"
        )
    statsRef' <- env statsRef -- get the statistics that Haxl collects
                              -- (has to be done in the `GenHaxl` monad)
                              -- or at least the IORef used to store it
    return (data', statsRef')
  statistics <- readIORef statsRef'
  putStrLn "Resulting data:"
  print $ catMaybes data'   -- Our data
  putStrLn "Statistics:"        -- some nice statistics. Number of rounds,
  putStrLn $ ppStats statistics -- number of fetches.
