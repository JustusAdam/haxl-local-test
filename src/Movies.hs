{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
module Movies (getActorForMovie, MovieReq, State(MovieReqState)) where


import           Data.Foldable       (for_)
import           Data.Hashable
import           Data.HashMap.Strict as HM
import           Data.Typeable
import           Haxl.Core


type Value = String
type Key = String

valueStore :: HashMap Key Value
valueStore = fromList
  [ ("Inception", "Leonardo di Caprio")
  , ("Avengers", "Robert Downey Jr")
  ]

getOneFromStore :: Key -> Maybe Value
getOneFromStore = flip HM.lookup valueStore

storeValFromFetch :: MovieReq a -> IO a
storeValFromFetch (GetActor key) = do
  putStrLn $ "Fetching movie object " ++ show key
  return $ getOneFromStore key


data MovieReq a where
  GetActor :: Key -> MovieReq (Maybe Value)

deriving instance Eq (MovieReq a)
deriving instance Show (MovieReq a)
instance Show1 MovieReq where show1 = show

instance DataSourceName MovieReq where
  dataSourceName _ = "movie"

instance StateKey MovieReq where
  data State MovieReq = MovieReqState


instance DataSource u MovieReq where
  fetch = storeFetch

instance Hashable (MovieReq a) where
  hashWithSalt s (GetActor k) = hashWithSalt s (0::Int, k)

storeFetch
  :: State MovieReq
  -> Flags
  -> u
  -> [BlockedFetch MovieReq]
  -> PerformFetch

storeFetch _ _ _ reqs =
  SyncFetch $ do
    putStrLn "Doing movie round"
    for_ reqs $ \(BlockedFetch fetch result) ->
      storeValFromFetch fetch >>= putSuccess result


getActorForMovie :: Key -> GenHaxl u (Maybe Value)
getActorForMovie = dataFetch . GetActor
