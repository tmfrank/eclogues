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

module Eclogues.Threads.Update (loadSchedulerState) where

import Eclogues.AppConfig (AppConfig)
import qualified Eclogues.AppConfig as Config
import qualified Eclogues.Persist as Persist
import Eclogues.Scheduling.Command (ScheduleConf (auroraURI), getSchedulerStatuses)
import Eclogues.State (activeJobs, updateJobs)
import qualified Eclogues.State.Monad as ES
import Eclogues.State.Types (AppState)
import Eclogues.Util (maybeDo)

import qualified Control.Concurrent.AdvSTM as STM
import qualified Control.Concurrent.AdvSTM.TChan as STM
import qualified Control.Concurrent.AdvSTM.TVar as STM
import Control.Exception (IOException, throwIO, try)
import Control.Lens ((^.))
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.HashMap.Lazy as HashMap
import System.IO (hPutStrLn, stderr)

-- | Query the scheduler and run 'updateJobs'.
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
            let (_, ts) = ES.runState state $ updateJobs aJobs newStatuses
            mapM_ (STM.writeTChan $ Config.schedChan conf) $ ts ^. ES.scheduleCommands
            STM.writeTVar stateV $ ts ^. ES.appState
            maybeDo $ STM.onCommit . Persist.atomically (Config.pctx conf) <$> ts ^. ES.persist
        Left (ex :: IOException)  ->
            hPutStrLn stderr $ "Error connecting to Aurora at " ++ show (auroraURI schedConf) ++ "; retrying: " ++ show ex
        Right (Left resp)         -> throwIO resp
