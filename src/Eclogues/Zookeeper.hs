{-# LANGUAGE TemplateHaskell #-}

module Eclogues.Zookeeper (ZKURI, ZookeeperError, getAuroraMaster, whenLeader, getLeaderInfo) where

import Control.Applicative ((<$>), pure)
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Arrow ((&&&))
import Control.Monad.Morph (squash)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), withExceptT, runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Aeson (eitherDecodeStrict)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString (ByteString)
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.Maybe (listToMaybe)
import Data.Word (Word16)
import Database.Zookeeper ( Zookeeper, Event (..), ZKError (..), AclList (..), CreateFlag (..)
                          , withZookeeper, getChildren, get, create )
import System.IO (hPutStrLn, stderr)

type ZKURI = String
type ZNode = String
type AuroraHost = (String, Word16)

data ZookeeperError e = ContentError e
                      | ZookeeperError ZKError
                      deriving (Show)

wrapE :: (Functor m) => (e -> e') -> m (Either e a) -> ExceptT e' m a
wrapE f = withExceptT f . ExceptT

wrapZKE :: (Functor m) => m (Either ZKError a) -> ExceptT (ZookeeperError e) m a
wrapZKE = wrapE ZookeeperError

data AuroraMember = AuroraMember { serviceEndpoint :: AuroraEndpoint } deriving (Show)
data AuroraEndpoint = AuroraEndpoint { host :: String, port :: Word16 } deriving (Show)

$(deriveJSON defaultOptions ''AuroraMember)
$(deriveJSON defaultOptions ''AuroraEndpoint)

electionPrefix :: String
electionPrefix = "member_"

maybeExists :: Either ZKError b -> Either ZKError (Maybe b)
maybeExists (Right a)          = Right (Just a)
maybeExists (Left NoNodeError) = Right Nothing
maybeExists (Left e)           = Left e

getMembers :: Zookeeper -> ZNode -> MaybeT (ExceptT (ZookeeperError e) IO) [String]
getMembers zk node = do
    children <- MaybeT . wrapZKE $ maybeExists <$> getChildren zk node Nothing
    pure . sort $ filter (isPrefixOf electionPrefix) children

getLeaderInfo :: forall e a. (Maybe ByteString -> Either e a) -> ZKURI -> ZNode -> ExceptT (ZookeeperError e) IO (Maybe a)
getLeaderInfo conv zkUri node = ExceptT $ withZookeeper zkUri 1000 Nothing Nothing (runExceptT . runMaybeT . fetch) where
    fetch :: Zookeeper -> MaybeT (ExceptT (ZookeeperError e) IO) a
    fetch zk = do
        first <- squash . MaybeT $ listToMaybe <$> getMembers zk node
        nodeM <- lift . wrapZKE $ maybeExists <$> get zk (node ++ "/" ++ first) Nothing
        case nodeM of
            Nothing       -> fetch zk
            Just (bsM, _) -> lift . withExceptT ContentError . ExceptT . pure $ conv bsM

getAuroraMaster :: ZKURI -> ZNode -> ExceptT (ZookeeperError String) IO (Maybe AuroraHost)
getAuroraMaster = getLeaderInfo conv where
    conv :: Maybe ByteString -> Either String AuroraHost
    conv Nothing   = Left "missing node content"
    conv (Just bs) = (host &&& port) . serviceEndpoint <$> eitherDecodeStrict bs

type ResVar a = MVar (Either (ZookeeperError ()) a)

whenLeader :: forall a. ZKURI -> ZNode -> ByteString -> IO a -> ExceptT (ZookeeperError ()) IO a
whenLeader zkUri node content act = ExceptT $ withZookeeper zkUri 1000 Nothing Nothing (runExceptT . run) where
    run :: Zookeeper -> ExceptT (ZookeeperError ()) IO a
    run zk = do
        mvar <- lift $ newEmptyMVar
        lift $ setup zk mvar
        ExceptT $ takeMVar mvar
    setup :: Zookeeper -> ResVar a -> IO ()
    setup zk mvar = errToVar mvar $ do
        wrapZKE $ ignoreExisting <$> create zk node Nothing OpenAclUnsafe []
        myNode <- wrapZKE $ create zk (node ++ '/':electionPrefix) (Just content) ReadAclUnsafe [Sequence, Ephemeral]
        lift . hPutStrLn stderr $ "Zookeeper node is " ++ myNode
        lift $ waitForLeader mvar zk myNode
    waitForLeader :: ResVar a -> Zookeeper -> String -> IO ()
    waitForLeader mvar zk myNode = errToVar mvar $ do
        membersM <- runMaybeT $ getMembers zk node
        lift . hPutStrLn stderr $ "Zookeeper members are " ++ show membersM
        lift $ case membersM of
            Just (first:_)
                | first `isSuffixOf` myNode -> do
                    hPutStrLn stderr "Elected as leader!"
                    putMVar mvar =<< Right <$> act
                | otherwise                 -> do
                    hPutStrLn stderr $ first ++ " elected as leader."
                    watch mvar zk myNode first
            _                               -> setup zk mvar  -- Somehow our node disappeared
    watch :: ResVar a -> Zookeeper -> String -> ZNode -> IO ()
    watch mvar zk myNode lowestNode = errToVar mvar $ do
        nodeM <- wrapZKE $ maybeExists <$> get zk (node ++ '/':lowestNode) (Just $ watcher mvar myNode)
        case nodeM of
            Nothing -> lift $ waitForLeader mvar zk myNode
            Just _  -> pure ()
    watcher mvar myNode zk DeletedEvent _ _ = waitForLeader mvar zk myNode
    watcher _    _      _  _            _ _ = pure ()
    ignoreExisting :: Either ZKError b -> Either ZKError ()
    ignoreExisting (Right _)              = Right ()
    ignoreExisting (Left NodeExistsError) = Right ()
    ignoreExisting (Left e)               = Left e
    errToVar :: ResVar a -> ExceptT (ZookeeperError ()) IO () -> IO ()
    errToVar mvar val = runExceptT val >>= \case
        Right () -> pure ()
        Left  e  -> putMVar mvar $ Left e
