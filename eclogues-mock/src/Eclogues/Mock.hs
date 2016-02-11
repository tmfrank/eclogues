{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Eclogues.Mock (MockHelper, main, runningMock, waitForUpdate) where

import Eclogues.API (AbsFile)
import Eclogues.AppConfig (AppConfig (AppConfig))
import qualified Eclogues.Job as Job
import Eclogues.Persist (withPersistDir)
import Eclogues.State (updateJobs)
import Eclogues.State.Monad (runState, appState)
import Eclogues.State.Types (AppState, jobs)
import Eclogues.Threads.Server (serve)

import Control.Concurrent (threadDelay)
import Control.Concurrent.AdvSTM (atomically)
import Control.Concurrent.AdvSTM.TVar (TVar, newTVar, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.AdvSTM.TChan (newTChanIO)
import Control.Concurrent.Async (waitAny, withAsync)
import qualified Control.Concurrent.Event as Event
import Control.Lens ((^.))
import Control.Monad (forever)
import Control.Monad.Trans (lift)
import Data.Default.Generics (def)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, listToMaybe)
import Data.Metrology ((%), (#))
import Data.Metrology.SI (Second (Second), micro)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.UUID (UUID)
import Data.Word (Word16)
import Network.URI (URI (uriPath), escapeURIString, isUnescapedInURI, parseURI)
import Path (toFilePath, mkAbsDir)
import Path.IO (withSystemTempDirectory)
import Text.Read (readMaybe)

data TargetStage = TargetStage Integer Job.Stage
data MockState = MockState (TVar AppState) (TVar (HM.HashMap Job.Name TargetStage))

main :: IO ()
main = run nope nope "127.0.0.1" 8000
  where
    nope = pure ()

run :: IO () -> IO () -> String -> Word16 -> IO ()
run bla ua host port' = withSystemTempDirectory "em" $ \d -> withPersistDir d $ \pctx -> lift $ do
    schedV <- newTChanIO
    let conf   = AppConfig jdir getURI schedV pctx fakeSchedulerJobUI outURI user (pure Nothing)
        jdir   = $(mkAbsDir "/mock/jobs")
        user   = "test"
        getURI = pure . Just . fromJust $ parseURI "http://localhost:8081/"
        outURI = mkOutputURI . fromJust $ parseURI "http://localhost:8001/"
        port   = fromIntegral port'
    mockState@(MockState stateV _) <- newMockState
    clusterV <- newTVarIO def
    let web = serve bla host port conf stateV clusterV
        updater = forever $ do
            update mockState
            ua
            threadDelay . floor $ ((1 % Second) # micro Second :: Double)

    putStrLn $ "Starting server on " ++ host ++ ':':show port
    withAsync web $ \webA -> withAsync updater $ \updaterA ->
        snd <$> waitAny [const undefined <$> webA, updaterA]

newMockState :: IO MockState
newMockState = atomically $ MockState <$> newTVar def <*> newTVar HM.empty

fakeSchedulerJobUI :: URI -> UUID -> URI
fakeSchedulerJobUI uri uuid = uri { uriPath = "/nonexistant-scheduler/" ++ show uuid }

update :: MockState -> IO ()
update (MockState stateV tgtStagesV) = atomically $ do
    state <- readTVar stateV
    tgtStages <- readTVar tgtStagesV
    let allJerbs               = state ^. jobs
        newJerbs               = HM.filter isLocalQueued allJerbs
        newTgtStages           = HM.mapWithKey (const . checkNamePrefix) newJerbs
        (tgtStages', updates)  = tickTargetStages $ tgtStages <> newTgtStages
        updatingJerbs          = HM.filterWithKey (willUpdate updates) allJerbs
        (_, ts)                = runState state $ updateJobs updatingJerbs updates
        state'                 = ts ^. appState
    writeTVar stateV state'
    writeTVar tgtStagesV tgtStages'
  where
    isLocalQueued = (== Job.Queued Job.LocalQueue) . (^. Job.stage)
    willUpdate updates name _stage = isJust $ lookup name updates

-- | Process the jobs to update their Job.Stage or TargetStage
-- if ticksLeft is 0, add to List of Job.Name and Job.Stage
-- if ticksLeft is greater than 0, decrement ticksLeft by 1 and add back to HashMap
tickTargetStages :: HM.HashMap Job.Name TargetStage -> (HM.HashMap Job.Name TargetStage, [(Job.Name, Job.Stage)])
tickTargetStages = HM.foldlWithKey' go (HM.empty, [])
  where
    go :: (HM.HashMap Job.Name TargetStage, [(Job.Name, Job.Stage)])
       -> Job.Name
       -> TargetStage
       -> (HM.HashMap Job.Name TargetStage, [(Job.Name, Job.Stage)])
    go (tss, us) name (TargetStage ticksLeft tgtStage)
        | ticksLeft == 0 = (tss, (name, tgtStage) : us)
        | otherwise = (HM.insert name (TargetStage (ticksLeft - 1) tgtStage) tss, us)

checkNamePrefix :: Job.Name -> TargetStage
checkNamePrefix name = fromMaybe defaultStage $ firstJust
    [ check "will_crash" (Job.Failed $ Job.NonZeroExitCode 42)
    , check "will_succeed" Job.Finished
    , check "user_killed" (Job.Failed Job.UserKilled)
    , check "memory_exceeded" (Job.Failed Job.MemoryExceeded)
    , check "disk_exceeded" (Job.Failed Job.DiskExceeded)
    , check "time_exceeded" (Job.Failed Job.TimeExceeded)
    , check "failed_dependency" (Job.Failed (Job.DependencyFailed $ forceName "failedDependency"))  ]
  where
    check :: T.Text -> Job.Stage -> Maybe TargetStage
    check = fin
    defaultStage = TargetStage 0 $ Job.Queued Job.SchedulerQueue
    mint pre = getTicksFromName pre name
    fin pre stage = TargetStage <$> mint (pre <> "_") <*> pure stage :: Maybe TargetStage
    firstJust :: [Maybe a] -> Maybe a
    firstJust = listToMaybe . catMaybes

getIntFromString :: T.Text -> Maybe Integer
getIntFromString strng = readMaybe . T.unpack $ head (T.split (=='_') strng)

-- | Parse a job name of the form "PREFIX_TICKS_REST" and return
-- the ticks iff the prefix matches. Returns 'Nothing' if the prefix
-- does not match or the name is not in the correct form, ie. the number
-- of ticks could not be parsed.
getTicksFromName :: T.Text -> Job.Name -> Maybe Integer
getTicksFromName pre stng = case T.stripPrefix pre (Job.nameText stng) of
  Just value -> getIntFromString value
  Nothing -> Nothing

-- TODO: move somewhere else
-- | Append the job name and file path to the path of the job output server URI.
mkOutputURI :: URI -> Job.Name -> AbsFile -> URI
mkOutputURI pf name path = pf { uriPath = uriPath pf ++ name' ++ escapedPath }
  where
    escapedPath = escapeURIString isUnescapedInURI $ toFilePath path
    name' = T.unpack $ Job.nameText name

forceName :: T.Text -> Job.Name
forceName jName = fromMaybe (error $ "invalid test name " ++ show jName) $ Job.mkName jName

newtype MockHelper = MockHelper Event.Event

runningMock :: String -> Word16 -> (MockHelper -> IO a) -> IO a
runningMock apiHost apiPort a = do
    go <- Event.new
    ev <- Event.new
    let onStart  = Event.set go
        onUpdate = Event.signal ev
    withAsync (run onStart onUpdate apiHost apiPort) . const $ do
        Event.wait go
        a $ MockHelper ev

waitForUpdate :: MockHelper -> IO ()
waitForUpdate (MockHelper ev) = Event.wait ev
