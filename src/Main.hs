{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Prelude hiding ((.))

import Eclogues.API (VAPI)
import Eclogues.AppConfig (AppConfig (AppConfig))
import Eclogues.Scheduling.Command (ScheduleCommand (GetStatuses), runScheduleCommand)
import Eclogues.State ( AppState, JobStatus (jobState), JobError (..), newAppState
                      , getJobs, activeJobs
                      , createJob, killJob, deleteJob, getJob, updateJobs )
import Eclogues.TaskSpec (JobState (..), FailureReason (..))
import Eclogues.Zookeeper (getAuroraMaster, whenLeader)
import Units

import Control.Applicative ((<$>), (<*), pure)
import Control.Category ((.))
import Control.Concurrent (threadDelay)
import Control.Concurrent.AdvSTM (atomically, onCommit)
import Control.Concurrent.AdvSTM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Concurrent.Async (race_)
import Control.Exception (Exception, throwIO)
import Control.Monad (forever, void)
import Control.Monad.Morph (hoist, generalize)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, ExceptT, runExceptT, withExceptT, mapExceptT, throwE)
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BC
import Data.HashMap.Lazy (keys)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text.Lazy as L
import Network.URI (parseURI)
import Network.Wai.Handler.Warp (run)
import Servant.API ((:<|>) ((:<|>)))
import Servant.Common.Text (FromText (..))
import Servant.Server (Server, ServerT, ServantErr (..), (:~>) (..)
                      , enter, fromExceptT
                      , serve, err404, err409)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

instance FromText L.Text where
    fromText = fmap L.fromStrict . fromText

bool :: a -> a -> Bool -> a
bool a b p = if p then b else a

throwExc :: (Exception e) => ExceptT e IO a -> IO a
throwExc act = runExceptT act >>= \case
    Left e  -> throwIO e
    Right a -> pure a

type Scheduler = AppState -> Except JobError (AppState, [ScheduleCommand ()])

runScheduler :: AppConfig -> TVar AppState -> Scheduler -> ExceptT JobError IO ()
runScheduler conf stateV f = mapExceptT atomically $ do
    state <- lift $ readTVar stateV
    (state', cmds) <- hoist generalize $ f state
    lift . onCommit . throwExc $ mapM_ (runScheduleCommand conf) cmds
    lift $ writeTVar stateV state'

mainServer :: AppConfig -> TVar AppState -> Server VAPI
mainServer conf stateV = enter (fromExceptT . Nat (withExceptT onError)) server where
    server :: ServerT VAPI (ExceptT JobError IO)
    server = getJobsH :<|> getJobH :<|> getJobStateH :<|> killJobH :<|> deleteJobH :<|> createJobH

    getJobsH = lift $ getJobs <$> atomically (readTVar stateV)
    getJobH jid = (hoist generalize . getJob jid) =<< lift (atomically $ readTVar stateV)
    getJobStateH = fmap jobState . getJobH
    createJobH = runScheduler' . createJob
    deleteJobH = runScheduler' . deleteJob

    killJobH jid (Failed UserKilled) = runScheduler' $ \st -> (st,) <$> killJob jid st
    killJobH jid _                   = throwE (InvalidStateTransition "Can only set state to Failed UserKilled") <* getJobH jid

    onError :: JobError -> ServantErr
    onError e = case e of
        NoSuchJob -> err404 { errBody = encode NoSuchJob }
        other     -> err409 { errBody = encode other }
    runScheduler' = runScheduler conf stateV

main :: IO ()
main = do
    (jobsDir:zkUri:myHost:_) <- getArgs
    auroraHostM <- runExceptT $ getAuroraMaster zkUri "/aurora/scheduler"
    let (host, port) = case auroraHostM of
            Right (Just hst) -> hst
            e                -> error (show e)
        (Just uri) = parseURI $ "http://" ++ host ++ ':':(show port) ++ "/api"
        conf = AppConfig jobsDir uri
    stateV <- atomically $ newTVar newAppState
    createDirectoryIfMissing False jobsDir

    hPutStrLn stderr $ "Found Aurora API at " ++ show uri

    let web = run 8000 $ serve (Proxy :: (Proxy VAPI)) $ mainServer conf stateV
        updater = forever $ goUpdate >> threadDelay (floor $ second (1 :: Double) `asVal` micro second)
        goUpdate = do
            -- TODO: does this race?
            state <- atomically $ readTVar stateV
            let aJobs = activeJobs state
            -- TODO: don't match on Right
            Right newStatuses <- runExceptT . runScheduleCommand conf . GetStatuses $ keys aJobs
            let (state', cmds) = updateJobs state aJobs newStatuses
            atomically $ do
                onCommit . throwExc $ mapM_ (runScheduleCommand conf) cmds
                writeTVar stateV state'
    void . runExceptT . withExceptT (error . show) . whenLeader zkUri "/eclogues" (BC.pack myHost) $ do
        hPutStrLn stderr "Starting server on port 8000"
        race_ web updater
