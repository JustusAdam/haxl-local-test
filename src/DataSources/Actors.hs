{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
module DataSources.Actors (getActor, StoreReq, State(StoreReqState)) where


import           Control.Concurrent  (threadDelay)
import           Data.Foldable       (for_)
import           Data.Hashable
import           Data.HashMap.Strict as HM
import           Data.Typeable
import           Haxl.Core
import           Report


type Value = String
type Key = String

valueStore :: HashMap Key Value
valueStore = fromList
  [ ("Harry Potter", "Daniel Radcliffe")
  , ("Hermoine Granger", "Emma Watson")
  , ("Ron Weasley", "Rupert Grint")
  , ("Minerva McGonagall", "Maggie Smith")
  ]

getOneFromStore :: Key -> Maybe Value
getOneFromStore = flip HM.lookup valueStore

storeValFromFetch :: StoreReq a -> IO a
storeValFromFetch (GetValue key) = do
  logLine $ "Fetching actor object " ++ show key
  return $ getOneFromStore key

data StoreReq a where
  GetValue :: Key -> StoreReq (Maybe Value)
  deriving Typeable

deriving instance Eq (StoreReq a)
deriving instance Show (StoreReq a)
instance Show1 StoreReq where show1 = show

instance DataSourceName StoreReq where
  dataSourceName _ = "store"

instance StateKey StoreReq where
  data State StoreReq = StoreReqState

instance DataSource u StoreReq where
  fetch = storeFetch

instance Hashable (StoreReq a) where
  hashWithSalt s (GetValue k) = hashWithSalt s (0::Int, k)

storeFetch
  :: State StoreReq
  -> Flags
  -> u
  -> [BlockedFetch StoreReq]
  -> PerformFetch

storeFetch _ _ _ reqs =
  SyncFetch $ do
    logLine "Doing actor round"
    threadDelay 100000
    for_ reqs $ \(BlockedFetch fetch result) ->
      storeValFromFetch fetch >>= putSuccess result
    logLine "Finished"


getActor :: Key -> GenHaxl u (Maybe Value)
getActor = dataFetch . GetValue
