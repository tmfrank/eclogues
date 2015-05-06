{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import AppConfig (AppConfig (AppConfig))
import Scheduling (ScheduleCommand (GetStatuses), runScheduleCommand)
import TaskAPI ( AppState, JobStatus (jobState), JobError (..), newAppState
               , createJob, killJob, deleteJob, getJob, getJobs, updateJobs, activeJobs )
import TaskSpec (TaskSpec (..), Name, JobState (..), FailureReason (..))
import Units

import Control.Applicative ((<$>), (<*), pure)
import Control.Concurrent (threadDelay)
import Control.Concurrent.AdvSTM (atomically, onCommit)
import Control.Concurrent.AdvSTM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Concurrent.Async (race_)
import Control.Exception (Exception, throwIO)
import Control.Monad (forever)
import Control.Monad.Morph (hoist, generalize)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT (..), left)
import Control.Monad.Trans.Except (Except, ExceptT, runExceptT, withExceptT, mapExceptT)
import Data.HashMap.Lazy (keys)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text.Lazy as L
import Network.URI (parseURI)
import Network.Wai.Handler.Warp (run)
import Servant.API ((:>), (:<|>) ((:<|>)), Get, Post, Put, Delete, ReqBody, Capture)
import Servant.Common.Text (FromText (..))
import Servant.Server (Server, serve)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

instance FromText L.Text where
    fromText = fmap L.fromStrict . fromText

type VAPI =  "jobs"   :> Get [JobStatus]
        :<|> "job"    :> Capture "id" Name :> Get JobStatus
        :<|> "job"    :> Capture "id" Name :> "state" :> Get JobState
        :<|> "job"    :> Capture "id" Name :> "state" :> ReqBody JobState :> Put ()
        :<|> "job"    :> Capture "id" Name :> Delete
        :<|> "create" :> ReqBody TaskSpec  :> Post ()

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

server :: AppConfig -> TVar AppState -> Server VAPI
server conf stateV = getJobsH :<|> getJobH :<|> getJobStateH :<|> killJobH :<|> deleteJobH :<|> createJobH where
    getJobsH = lift $ getJobs <$> atomically (readTVar stateV)
    getJobH jid = toEitherT . withExceptT onError $ (hoist generalize . getJob jid) =<< lift (atomically $ readTVar stateV)
    getJobStateH = fmap jobState . getJobH
    createJobH = runScheduler' . createJob
    deleteJobH = runScheduler' . deleteJob

    killJobH jid (Failed UserKilled) = runScheduler' $ \st -> (st,) <$> killJob jid st
    killJobH jid _                   = left (409, "Can only set state to Failed UserKilled") <* getJobH jid

    onError e = case e of
        UnexpectedResponse res   -> (500, show res)
        NoSuchJob                -> (404, "")
        JobMustBeTerminated yn   -> (409, "Job " ++ bool "must not" "must" yn ++ " be terminated")
        JobMustExist name        -> (409, "Job " ++ L.unpack name ++ " must already exist")
        JobCannotHaveFailed name -> (409, "Job " ++ L.unpack name ++ " cannot have failed")
        JobNameUsed              -> (409, "Job name already used")
        OutstandingDependants l  -> (409, "Job has outstanding dependants " ++ show l)
    toEitherT = EitherT . runExceptT
    runScheduler' = toEitherT . withExceptT onError . runScheduler conf stateV

main :: IO ()
main = do
    (jobsDir:hostArg:_) <- getArgs
    let (Just uri) = parseURI $ "http://" ++ hostArg ++ "/api"
        conf = AppConfig jobsDir uri
    stateV <- atomically $ newTVar newAppState
    createDirectoryIfMissing False jobsDir

    hPutStrLn stderr "Starting server on port 8000"

    let web = run 8000 $ serve (Proxy :: (Proxy VAPI)) $ server conf stateV
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
    race_ web updater
