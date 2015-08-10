{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

Election pattern for Zookeeper.
-}

module Database.Zookeeper.Election ( ZookeeperError, LeadershipError (..)
                                   , whenLeader, getLeaderInfo, followLeaderInfo )
                                   where

import Database.Zookeeper.ManagedEvents (ZNode, ManagedZK (ManagedZK), ZKEvent (..))

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, tryPutMVar, tryTakeMVar)
import Control.Concurrent.AdvSTM (atomically)
import Control.Concurrent.AdvSTM.TVar (TVar, writeTVar, readTVar)
import Control.Concurrent.Async (Async, async, waitCatch, race, waitAnyCancel, cancel)
import Control.Concurrent.Broadcast (listen)
import Control.Exception.Lifted (SomeException, finally)
import Control.Monad (void, join)
import Control.Monad.Morph (squash, hoist)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), withExceptT, runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.ByteString (ByteString)
import Data.Either.Combinators (mapLeft)
import Data.List (isPrefixOf, sort, stripPrefix)
import Data.Maybe (listToMaybe)
import Database.Zookeeper ( Zookeeper, Event (..), State (..), ZKError (..), AclList (..), CreateFlag (..), Watcher
                          , getChildren, get, create, delete, getState )
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

-- | Follow a Zookeeper election, updating a TVar with details of the
-- current leader (if any).
followLeaderInfo :: ManagedZK
                 -> ZNode -- ^ The node under which the election is taking place
                 -> TVar (Maybe (Maybe ByteString)) -- ^ The TVar to update with leader details. There may not be
                                                    -- a current leader (outer Maybe), and that leader node may or
                                                    -- may not contain data (inner Maybe)
                 -> IO ZKError -- ^ This function only returns if there is a ZK error
followLeaderInfo (ManagedZK zk br) node var = run where
    run = do
        errVar <- newEmptyMVar
        errE <- flip finally zeroVar . race (gWatcher errVar) $ do
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
    zeroVar = atomically $ writeTVar var Nothing
    gWatcher errVar = listen br >>= \case
        ZKEvent SessionEvent ExpiredSessionState _ -> pure SessionExpiredError
        ZKEvent SessionEvent ConnectingState     _ -> zeroVar *> gWatcher errVar
        ZKEvent SessionEvent ConnectedState      _ -> follow errVar *> gWatcher errVar
        Disconnected                               -> pure ConnectionLossError
        _                                          -> gWatcher errVar

-- TODO: remove ZookeeperError
-- | Get the current leader of an election, if any such node exists.
getLeaderInfo :: forall e a.
       (Maybe ByteString -> Either e a) -- ^ A conversion function for leader node data
    -> ManagedZK
    -> ZNode -- ^ The node under which the election is taking place
    -> ExceptT (ZookeeperError e) IO (Maybe a)
getLeaderInfo conv (ManagedZK zk _) node = runMaybeT fetch where
    fetch :: MaybeT (ExceptT (ZookeeperError e) IO) a
    fetch = do
        first <- squash . MaybeT $ listToMaybe <$> (hoist (withExceptT ZookeeperError) $ getMembers zk node Nothing)
        nodeM <- lift . wrapZKE $ maybeExists <$> get zk (node ++ "/" ++ first) Nothing
        case nodeM of
            Nothing       -> fetch
            Just (bsM, _) -> lift . withExceptT ContentError . ExceptT . pure $ conv bsM
    wrapZKE :: forall e' a' m. (Functor m) => m (Either ZKError a') -> ExceptT (ZookeeperError e') m a'
    wrapZKE = withExceptT ZookeeperError . ExceptT

ensureEphemeralNodeDeleted :: ManagedZK -> ZNode -> IO (Async (Maybe ZKError))
ensureEphemeralNodeDeleted (ManagedZK zk br) node = async attempt where
    attempt = delete zk node Nothing >>= \case
        Left ConnectionLossError -> watch
        Left SessionExpiredError -> pure Nothing
        Left NoNodeError         -> pure Nothing
        Left e                   -> pure $ Just e
        Right ()                 -> pure Nothing
    watch = listen br >>= \case
        ZKEvent SessionEvent ExpiredSessionState _ -> pure Nothing
        ZKEvent SessionEvent ConnectedState      _ -> attempt
        Disconnected                               -> pure Nothing
        _                                          -> watch

data LeadershipError = LZKError ZKError
                     | ActionException SomeException
                     | LeadershipLost
                     deriving (Show)

type NodeBasename = String

findPrev :: (Eq a) => a -> [a] -> Maybe a
findPrev a (x:nx:xs)
    | nx == a   = Just x
    | otherwise = findPrev a xs
findPrev _ _    = Nothing

-- | Run some IO action only when elected.
--
-- Exceptions from the action will be returned in 'ActionException'.
whenLeader :: forall a. ManagedZK
           -> ZNode      -- ^ The node under which the election is taking place
           -> ByteString -- ^ Data for election node
           -> IO a       -- ^ Action to run if elected
           -> ExceptT LeadershipError IO a
whenLeader mzk@(ManagedZK zk br) node content act = whenConnected where
    whenConnected = (lift $ getState zk) >>= \case
        ConnectedState -> run
        _              -> lift (listen br) *> whenConnected
    run :: ExceptT LeadershipError IO a
    run = do
        errVar <- lift newEmptyMVar
        actVar <- lift newEmptyMVar
        let vars = (errVar, actVar)
        (myNode, myNodeAbs) <- withExceptT LZKError setup
        finally (startTracking vars myNode) (lift $ ensureEphemeralNodeDeleted mzk myNodeAbs)
    setup :: ExceptT ZKError IO (NodeBasename, ZNode)
    setup = do
        ExceptT $ ignoreExisting <$> create zk node Nothing OpenAclUnsafe []
        myNodeAbs <- ExceptT $ create zk (node ++ '/':electionPrefix) (Just content) ReadAclUnsafe [Sequence, Ephemeral]
        let Just (_:myNode) = stripPrefix node myNodeAbs
        lift . hPutStrLn stderr $ "Zookeeper node is " ++ myNode
        pure (myNode, myNodeAbs)
    startTracking :: (MVar ZKError, MVar (Async a)) -> NodeBasename -> ExceptT LeadershipError IO a
    startTracking vars myNode = do
        withExceptT LZKError $ waitForLeader vars myNode
        let (errVar, actVar) = vars
        res <- lift . fmap snd $ waitAnyCancel =<< mapM async
            [ Left . LZKError <$> takeMVar errVar
            , mapLeft ActionException <$> (waitCatch =<< takeMVar actVar)
            , Left <$> watchForLoss ]
        lift $ tryTakeMVar actVar >>= \case
            Just asyn -> cancel asyn
            Nothing   -> pure ()
        ExceptT $ pure res
    waitForLeader :: (MVar ZKError, MVar (Async a)) -> NodeBasename -> ExceptT ZKError IO ()
    waitForLeader vars myNode = do
        membersM <- runMaybeT $ getMembers zk node Nothing
        lift . hPutStrLn stderr $ "Zookeeper members are " ++ show membersM
        case membersM of
            Just ms@(first:_)
                | first == myNode                 -> lift $ do
                    hPutStrLn stderr "Elected as leader!"
                    putMVar (snd vars) =<< async act
                    -- TODO: delete leadership node
                | Just prev <- findPrev myNode ms -> do
                    lift . hPutStrLn stderr $ first ++ " elected as leader."
                    watch vars myNode prev
            _ -> error "impossibru! somehow our node disappeared!"
    watch :: (MVar ZKError, MVar (Async a)) -> NodeBasename -> NodeBasename -> ExceptT ZKError IO ()
    watch vars myNode lowestNode = do
        nodeM <- ExceptT $ maybeExists <$> get zk (node ++ '/':lowestNode) (Just $ watcher vars myNode)
        case nodeM of
            Nothing -> waitForLeader vars myNode
            Just _  -> pure ()
    watcher vars myNode _ DeletedEvent _ _ = errToVar (fst vars) $ waitForLeader vars myNode
    watcher _    _      _ _            _ _ = pure ()
    ignoreExisting :: Either ZKError b -> Either ZKError ()
    ignoreExisting (Right _)              = Right ()
    ignoreExisting (Left NodeExistsError) = Right ()
    ignoreExisting (Left e)               = Left e
    errToVar :: MVar ZKError -> ExceptT ZKError IO () -> IO ()
    errToVar mvar val = runExceptT val >>= \case
        Right () -> pure ()
        Left  e  -> putMVar mvar e
    watchForLoss :: IO LeadershipError
    watchForLoss = listen br >>= \case
        ZKEvent SessionEvent ExpiredSessionState _ -> pure LeadershipLost
        ZKEvent SessionEvent ConnectingState     _ -> pure LeadershipLost
        _                                          -> watchForLoss
