{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

Zookeeper connection with broadcasted events, so more than one function can
react.

See also 'ZK.withZookeeper'.
-}

module Database.Zookeeper.ManagedEvents where

import Control.Concurrent.Broadcast (Broadcast)
import qualified Control.Concurrent.Broadcast as Broadcast
import Control.Exception (finally)
import qualified Database.Zookeeper as ZK

type ZKURI = String
type ZNode = String

data ZKEvent = ZKEvent ZK.Event ZK.State (Maybe ZNode)
             | Disconnected
             deriving (Eq, Show)

data ManagedZK = ManagedZK { mzk      :: ZK.Zookeeper
                           , zkEvents :: Broadcast ZKEvent }

withZookeeper :: ZKURI -> (ManagedZK -> IO a) -> IO a
withZookeeper uri = withZookeeper' uri 1000 Nothing

withZookeeper' :: ZKURI -> ZK.Timeout -> Maybe ZK.ClientID -> (ManagedZK -> IO a) -> IO a
withZookeeper' uri timeout clid act = go where
    go = do
        br <- Broadcast.new
        let act' zk = finally (act $ ManagedZK zk br) (broadcastEnd br)
        ZK.withZookeeper uri timeout (Just $ dispatch br) clid act'
    dispatch br _ ZK.SessionEvent ZK.ExpiredSessionState _ =
        Broadcast.broadcast br $ ZKEvent ZK.SessionEvent ZK.ExpiredSessionState Nothing
    dispatch br _ ev              st                     n =
        Broadcast.signal br $ ZKEvent ev st n
    broadcastEnd br = Broadcast.broadcast br Disconnected
