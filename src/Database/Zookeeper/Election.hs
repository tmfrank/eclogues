{-# LANGUAGE LambdaCase #-}

module Database.Zookeeper.Election (ZKURI, ZNode, ZookeeperError, whenLeader, getLeaderInfo, followLeaderInfo) where

import Database.Zookeeper.ManagedEvents (ZKURI, ZNode, ManagedZK (..), ZKEvent (ZKEvent))

import Control.Applicative ((<$>), (*>), pure)
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Concurrent.AdvSTM (atomically)
import Control.Concurrent.AdvSTM.TVar (TVar, writeTVar, readTVar)
import Control.Concurrent.Async (race)
import Control.Concurrent.Broadcast (listen)
import Control.Monad (void, join)
import Control.Monad.Morph (squash, hoist)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), withExceptT, runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.ByteString (ByteString)
import Data.List (isPrefixOf, sort, stripPrefix)
import Data.Maybe (listToMaybe)
import Database.Zookeeper ( Zookeeper, Event (..), State (..), ZKError (..), AclList (..), CreateFlag (..), Watcher
                          , withZookeeper, getChildren, get, create )
import System.IO (hPutStrLn, stderr)

data ZookeeperError e = ContentError e
                      | ZookeeperError ZKError
                      deriving (Show)

electionPrefix :: String
electionPrefix = "member_"

maybeExists :: Either ZKError b -> Either ZKError (Maybe b)
maybeExists (Right a)          = Right (Just a)
maybeExists (Left NoNodeError) = Right Nothing
maybeExists (Left e)           = Left e

getMembers :: Zookeeper -> ZNode -> Maybe Watcher -> MaybeT (ExceptT ZKError IO) [String]
getMembers zk node watch = do
    children <- MaybeT . ExceptT $ maybeExists <$> getChildren zk node watch
    pure . sort $ filter (isPrefixOf electionPrefix) children

followLeaderInfo :: ManagedZK -> ZNode -> TVar (Maybe (Maybe ByteString)) -> IO ZKError
followLeaderInfo (ManagedZK zk br) node var = run where
    run = do
        errVar <- newEmptyMVar
        errE <- race (gWatcher errVar) $ do
            follow errVar
            takeMVar errVar
        pure $ either id id errE
    follow errVar = do
        hPutStrLn stderr $ "Looking up leader info for " ++ node
        firstME <- runExceptT . fmap join . runMaybeT $ listToMaybe <$> getMembers zk node (Just $ nodeWatcher errVar)
        case firstME of
            Left e          -> void $ tryPutMVar errVar e
            Right Nothing   -> pure ()
            Right (Just nn) -> followNode errVar (node ++ "/" ++ nn)
    followNode errVar fullPath = do
        nodeME <- maybeExists <$> get zk fullPath (Just $ nodeWatcher errVar)
        case nodeME of
            Left  e               -> void $ tryPutMVar errVar e
            Right Nothing         -> follow errVar
            Right (Just (bsM, _)) -> do
                hPutStrLn stderr $ "Found leader for " ++ node
                atomically . writeTVar var $ Just bsM
    nodeWatcher errVar _ ChildEvent _ _ = atomically (readTVar var) >>= \case
        Nothing -> follow errVar
        Just _  -> pure ()
    nodeWatcher errVar _ DeletedEvent _  _       = atomically (writeTVar var Nothing) *> follow errVar
    nodeWatcher errVar _ ChangedEvent _ (Just n) = followNode errVar n
    nodeWatcher _      _ _            _ _        = pure ()
    gWatcher errVar = listen br >>= \case
        ZKEvent SessionEvent ExpiredSessionState _ -> pure SessionExpiredError
        ZKEvent SessionEvent ConnectingState     _ -> do
            atomically (writeTVar var Nothing)
            hPutStrLn stderr "Reconnecting..."
            gWatcher errVar
        ZKEvent SessionEvent ConnectedState      _ -> follow errVar *> gWatcher errVar
        _                                          -> pure () *> gWatcher errVar

-- TODO: take ManagedZK
getLeaderInfo :: forall e a. (Maybe ByteString -> Either e a) -> ZKURI -> ZNode -> ExceptT (ZookeeperError e) IO (Maybe a)
getLeaderInfo conv zkUri node = ExceptT $ withZookeeper zkUri 1000 Nothing Nothing (runExceptT . runMaybeT . fetch) where
    fetch :: Zookeeper -> MaybeT (ExceptT (ZookeeperError e) IO) a
    fetch zk = do
        first <- squash . MaybeT $ listToMaybe <$> (hoist (withExceptT ZookeeperError) $ getMembers zk node Nothing)
        nodeM <- lift . wrapZKE $ maybeExists <$> get zk (node ++ "/" ++ first) Nothing
        case nodeM of
            Nothing       -> fetch zk
            Just (bsM, _) -> lift . withExceptT ContentError . ExceptT . pure $ conv bsM
    wrapZKE :: forall e' a' m. (Functor m) => m (Either ZKError a') -> ExceptT (ZookeeperError e') m a'
    wrapZKE = withExceptT ZookeeperError . ExceptT

type ResVar a = MVar (Either ZKError a)

findPrev :: (Eq a) => a -> [a] -> Maybe a
findPrev a (x:nx:xs)
    | nx == a   = Just x
    | otherwise = findPrev a xs
findPrev _ _    = Nothing

whenLeader :: forall a. ManagedZK -> ZNode -> ByteString -> IO a -> ExceptT ZKError IO a
whenLeader (ManagedZK zk _) node content act = run where
    run :: ExceptT ZKError IO a
    run = do
        mvar <- lift $ newEmptyMVar
        lift $ setup mvar
        ExceptT $ takeMVar mvar
    setup :: ResVar a -> IO ()
    setup mvar = errToVar mvar $ do
        ExceptT $ ignoreExisting <$> create zk node Nothing OpenAclUnsafe []
        myNodeAbs <- ExceptT $ create zk (node ++ '/':electionPrefix) (Just content) ReadAclUnsafe [Sequence, Ephemeral]
        let Just (_:myNode) = stripPrefix node myNodeAbs
        lift . hPutStrLn stderr $ "Zookeeper node is " ++ myNode
        lift $ waitForLeader mvar myNode
    waitForLeader :: ResVar a -> String -> IO ()
    waitForLeader mvar myNode = errToVar mvar $ do
        membersM <- runMaybeT $ getMembers zk node Nothing
        lift . hPutStrLn stderr $ "Zookeeper members are " ++ show membersM
        lift $ case membersM of
            Just ms@(first:_)
                | first == myNode                 -> do
                    hPutStrLn stderr "Elected as leader!"
                    putMVar mvar =<< Right <$> act
                | Just prev <- findPrev myNode ms -> do
                    hPutStrLn stderr $ first ++ " elected as leader."
                    watch mvar myNode prev
            _                               -> setup mvar  -- Somehow our node disappeared
    watch :: ResVar a -> String -> ZNode -> IO ()
    watch mvar myNode lowestNode = errToVar mvar $ do
        nodeM <- ExceptT $ maybeExists <$> get zk (node ++ '/':lowestNode) (Just $ watcher mvar myNode)
        case nodeM of
            Nothing -> lift $ waitForLeader mvar myNode
            Just _  -> pure ()
    watcher mvar myNode _ DeletedEvent _ _ = waitForLeader mvar myNode
    watcher _    _      _ _            _ _ = pure ()
    ignoreExisting :: Either ZKError b -> Either ZKError ()
    ignoreExisting (Right _)              = Right ()
    ignoreExisting (Left NodeExistsError) = Right ()
    ignoreExisting (Left e)               = Left e
    errToVar :: ResVar a -> ExceptT ZKError IO () -> IO ()
    errToVar mvar val = runExceptT val >>= \case
        Right () -> pure ()
        Left  e  -> putMVar mvar $ Left e
