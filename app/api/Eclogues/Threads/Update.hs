{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

Updating local state with data from the scheduler.
-}

module Eclogues.Threads.Update (loadSchedulerState, monitorCluster) where

import Eclogues.AppConfig (AppConfig)
import qualified Eclogues.AppConfig as Config
import qualified Eclogues.Persist as Persist
import Eclogues.Scheduling.Command (ScheduleConf (auroraURI), getSchedulerStatuses)
import Eclogues.State (activeJobs, pendingJobs, updateJobStates, updateJobSatis)
import qualified Eclogues.State.Monad as ES
import Eclogues.State.Types (AppState)
import Eclogues.Job as Job
import Eclogues.Util (maybeDo)

import qualified Control.Concurrent.AdvSTM as STM
import qualified Control.Concurrent.AdvSTM.TChan as STM
import qualified Control.Concurrent.AdvSTM.TVar as STM
import Control.Exception (IOException, throwIO, try)
import Control.Lens (view, (^.))
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.HashMap.Lazy as HashMap
import System.IO (hPutStrLn, stderr)
import Eclogues.Monitoring.Cluster as CM
import Eclogues.Monitoring.Graphite as Graphite

-- | Query the scheduler and run 'updateJobStates'.
loadSchedulerState :: AppConfig -> STM.TVar AppState -> IO ()
loadSchedulerState conf stateV = do
    schedConf <- STM.atomically $ Config.requireSchedConf conf
    (newStatusesRes, aJobs) <- do
        state <- STM.atomically $ STM.readTVar stateV
        let aJobs = activeJobs state
        newStatusesRes <- try . runExceptT . getSchedulerStatuses schedConf $ HashMap.elems aJobs
        pure (newStatusesRes, aJobs)
    case newStatusesRes of
        Right (Right newStatuses) -> STM.atomically $ do
            state <- STM.readTVar stateV
            let (_, ts) = ES.runState state $ updateJobStates aJobs newStatuses
            mapM_ (STM.writeTChan $ Config.schedChan conf) $ ts ^. ES.scheduleCommands
            STM.writeTVar stateV $ ts ^. ES.appState
            maybeDo $ STM.onCommit . Persist.atomically (Config.pctx conf) <$> ts ^. ES.persist
        Left (ex :: IOException)  ->
            hPutStrLn stderr $ "Error connecting to Aurora at " ++ show (auroraURI schedConf) ++ "; retrying: " ++ show ex
        Right (Left resp)         -> throwIO resp

-- | Obtain estimated resources for all available sources and update job satisfiability.
monitorCluster :: AppConfig -> STM.TVar AppState -> STM.TVar (Maybe CM.Cluster) -> IO ()
monitorCluster conf stateV clusterV = do
    let satisfiabilityUnknown = HashMap.toList . HashMap.map (const Job.SatisfiabilityUnknown)
    let possiblySatisfiable clstr = HashMap.toList . HashMap.map (satisfiability clstr . view Job.spec)
    clusterRes <- try . runReaderT Graphite.getCluster $ Config.graphiteUrl conf
    getSatis <- case clusterRes of
        Right cluster -> do
            STM.atomically $ STM.writeTVar clusterV (Just cluster)
            return $ possiblySatisfiable cluster
        Left (ex :: IOException) -> do
            STM.atomically $ STM.writeTVar clusterV Nothing
            hPutStrLn stderr $ "Error connecting to Graphite at " ++ Config.graphiteUrl conf ++ "; retrying: " ++ show ex
            return satisfiabilityUnknown
    STM.atomically $ do
        state <- STM.readTVar stateV
        let pJobs = pendingJobs state
        let newSatis = updateJobSatis pJobs (getSatis pJobs)
        let (_, ts) = ES.runState state newSatis
        STM.writeTVar stateV $ ts ^. ES.appState
