module Database.Zookeeper.ManagedEvents where

import Control.Concurrent.Broadcast (Broadcast)
import qualified Control.Concurrent.Broadcast as Broadcast
import qualified Database.Zookeeper as ZK

type ZKURI = String
type ZNode = String

data ZKEvent = ZKEvent { event :: ZK.Event
                       , state :: ZK.State
                       , znode :: Maybe ZNode }
                       deriving (Eq, Show)

data ManagedZK = ManagedZK { mzk      :: ZK.Zookeeper
                           , zkEvents :: Broadcast ZKEvent }

withZookeeper :: ZKURI -> (ManagedZK -> IO a) -> IO a
withZookeeper uri act = withZookeeper' uri 1000 Nothing act

withZookeeper' :: ZKURI -> ZK.Timeout -> Maybe ZK.ClientID -> (ManagedZK -> IO a) -> IO a
withZookeeper' uri timeout clid act = go where
    go = do
        broadcast <- Broadcast.new
        let act' zk = act $ ManagedZK zk broadcast
        ZK.withZookeeper uri timeout (Just $ dispatch broadcast) clid act'
    dispatch br _ ZK.SessionEvent ZK.ExpiredSessionState _ =
        Broadcast.broadcast br $ ZKEvent ZK.SessionEvent ZK.ExpiredSessionState Nothing
    dispatch br _ ev              st                     n =
        Broadcast.signal br $ ZKEvent ev st n
