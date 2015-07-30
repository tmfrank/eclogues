{-# LANGUAGE OverloadedStrings #-}

module StateSpec where

import Eclogues.JobSpec
import Eclogues.State (createJob, killJob, deleteJob, activeJobs, updateJobs)
import Eclogues.State.Types
import qualified Eclogues.State.Monad as ES
import Eclogues.API (JobError(..))
import Units

import Test.Hspec
import Data.Default.Generics (def)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Either (isRight)
import Data.UUID (nil)
import Data.HashMap.Lazy (keys, mapWithKey, fromList, elems)
import qualified Data.HashMap.Lazy as HashMap (filter)
import Control.Lens ((^.), (.~), at)
import Data.Maybe (isNothing)

maybeOrFalse :: (a -> Bool) -> Maybe a -> Bool
maybeOrFalse = maybe False

defaultState :: AppState
defaultState = def

isJobError :: JobError -> Either JobError a -> Bool
isJobError x = either ((==) x) (const False)

isJobInState :: JobState -> JobStatus -> Bool
isJobInState state status = status ^. jobState == state

run :: ExceptT JobError ES.EState () -> (Either JobError (), AppState)
run r = (x, y ^. appState)
    where (x, y) = ES.runEState defaultState . runExceptT $ r

outputRun :: ExceptT JobError ES.EState () -> Either JobError ()
outputRun = fst . run

effectRun :: ExceptT JobError ES.EState () -> AppState
effectRun = snd . run

isolatedJob :: Name -> JobSpec
isolatedJob x = JobSpec x "/bin/echo" res [] False []
    where res = Resources (mega byte 10) (mebi byte 10) (core 0.1) (second 5)

dependentJob :: Name -> [Name] -> JobSpec
dependentJob job deps = dependsOn .~ deps $ isolatedJob job

lookupJob :: Name -> AppState -> Maybe (JobStatus)
lookupJob name state = state ^. appState ^. jobs ^. at name

lookupRevDep :: Name -> AppState -> Maybe [Name]
lookupRevDep name state = state ^. appState ^. revDeps ^. at name

createJob' :: JobSpec -> ExceptT JobError ES.EState ()
createJob' = createJob nil

createJobInState :: Name -> JobState -> ExceptT JobError ES.EState ()
createJobInState name state = do
    createJob' $ isolatedJob name
    lift $ ES.setJobState name state

testCreateJob :: Spec
testCreateJob = do
    describe "createJob" $
        it "should succeed when given a unique, valid job" $
            let result = outputRun . createJob' $ isolatedJob "hello"
            in result `shouldSatisfy` isRight

    context "when provided with dependency in Finished state" $
        it "should be placed in the Queued state" $
            let result = effectRun $ do
                    createJobInState "dep" Finished
                    createJob' $ dependentJob "hello" ["dep"]
                criterion = maybeOrFalse . isJobInState $ Queued LocalQueue
            in lookupJob "hello" result `shouldSatisfy` criterion

    context "when provided with dependency in an active state" $
        it "should be waiting on one job" $
            let result = effectRun $ do
                    createJobInState "dep" Running
                    createJob' $ dependentJob "hello" ["dep"]
                criterion = maybeOrFalse . isJobInState $ Waiting 1
            in lookupJob "hello" result `shouldSatisfy` criterion

    context "when provided with a dependency" $
        it "should update dependants list for that dependency" $
            let result = effectRun $ do
                    createJob' $ isolatedJob "dep"
                    createJob' $ dependentJob "hello" ["dep"]
                criterion = maybeOrFalse $ (==) ["hello"]
            in lookupRevDep "dep" (result ^. appState) `shouldSatisfy` criterion

    context "when provided a job with a name that already exists" $
        it "should return JobNameUsed error" $
            let result = outputRun $ do
                    createJob' $ isolatedJob "hello"
                    createJob' $ isolatedJob "hello"
            in result `shouldSatisfy` isJobError JobNameUsed

    context "when provided a dependency that doesn't exist" $
        it "should return JobMustExist error" $
            let result = outputRun $ createJob' $ dependentJob "hello" ["dep"]
            in result `shouldSatisfy` isJobError (JobMustExist "dep")

    context "when provided a dependency that has failed" $
        it "should return JobCannotHaveFailed error" $
            let result = outputRun $ do
                    createJobInState "dep" (Failed UserKilled)
                    createJob' $ dependentJob "hello" ["dep"]
            in result `shouldSatisfy` isJobError (JobCannotHaveFailed "dep")

testKillJob :: Spec
testKillJob = do
    describe "killJob" $
        it "should transition job with given name to Killing state" $
            let result = effectRun $ do
                    createJob' $ isolatedJob "hello"
                    killJob "hello"
                criterion = maybeOrFalse . isJobInState $ Killing
            in lookupJob "hello" result `shouldSatisfy` criterion

    context "when provided the name of a job that doesn't exist" $
        it "should return NoSuchJob error" $
            let result = outputRun $ killJob "hello"
            in result `shouldSatisfy` isJobError NoSuchJob

    context "when provided the name of a job that is in a termination state" $
        it "should return JobMustBeTerminated error" $
            let result = outputRun $ do
                    createJobInState "hello" Finished
                    killJob "hello"
            in result `shouldSatisfy` isJobError (JobMustBeTerminated False)

testDeleteJob :: Spec
testDeleteJob = do
    it "should remove the finished job with the given name from application state" $
        let result = effectRun $ do
                createJobInState "hello" Finished
                deleteJob "hello"
        in lookupJob "hello" result `shouldSatisfy` isNothing

    context "when provided the name of a job in an active state" $
        it "should return JobMustBeTerminated error" $
            let result = outputRun $ do
                    createJobInState "hello" Running
                    deleteJob "hello"
            in result `shouldSatisfy` isJobError (JobMustBeTerminated True)

    context "when provided the name of a job that has outstanding dependants" $
        it "should return OutstandingDependants error" $
            let result = outputRun $ do
                    createJobInState "dep" Finished
                    createJob' $ dependentJob "hello" ["dep"]
                    deleteJob "dep"
            in result `shouldSatisfy` isJobError (OutstandingDependants ["hello"])

testActiveJobs :: Spec
testActiveJobs = do
    describe "activeJobs" $
        it "should return all jobs that are in an active state (excluding waiting jobs)" $
            let testJobs = fromList $
                    [ ("queued", Queued LocalQueue)
                    , ("waiting", Waiting 0)
                    , ("running", Running)
                    , ("killing", Killing)
                    , ("finished", Finished)
                    , ("failed", Failed UserKilled)
                    , ("runerror", RunError SchedulerLost)
                    ]
                result = effectRun . sequence_ . elems $ mapWithKey createJobInState testJobs
                criterion = keys $ HashMap.filter isActive testJobs
                isActive s = case s of { Waiting _ -> False; x -> isActiveState x }
            in keys (activeJobs result) `shouldBe` criterion

testUpdateJobs :: Spec
testUpdateJobs = let
        appStateFromDepJob :: Name -> Name -> ExceptT JobError ES.EState ()
        appStateFromDepJob depName jobName = do
            createJob' $ isolatedJob depName
            createJob' $ dependentJob jobName [depName]

        mockStateFromDepJob :: Name -> JobState -> Name -> JobState -> AppState
        mockStateFromDepJob depName depState jobName' jobState' = effectRun $ do
            createJob' $ isolatedJob depName
            createJob' $ dependentJob jobName' [depName]
            lift $ ES.setJobState jobName' jobState'
            lift $ ES.setJobState depName depState

        testUpdate :: JobState -> Maybe JobState -> JobState -> Expectation
        testUpdate oldState newState expectedState = result `shouldBe` expected
            where oldAppState = effectRun $ createJobInState "job" oldState
                  update = updateJobs (oldAppState ^. jobs) $ statuses
                  statuses = maybe [] (\x -> [("job", x)]) newState
                  newAppState = snd . (ES.runEState oldAppState) $ update
                  result = newAppState ^. ES.appState
                  expected = effectRun $ createJobInState "job" expectedState
    in do
        describe "updateJobs" $
            it "should do nothing when job status hasn't changed" $
                let inState = Queued LocalQueue
                in testUpdate inState (Just inState) inState

        context "when job is on scheduler and no new status information is received" $
            it "should do nothing" $
                testUpdate (Queued LocalQueue) Nothing (Queued LocalQueue)

        context "when job is in state Killing and no updated information is provided" $
            it "should change job to Failed UserKilled state" $
                testUpdate Killing Nothing (Failed UserKilled)

        context "when job is in state Killing and is updated to state Finished" $
            it "should change job to Failed UserKilled state" $
                testUpdate Killing (Just Finished) (Failed UserKilled)

        context "when job is in state Killing and is updated to state RunError SchedulerLost" $
            it "should change job to RunError SchedulerLost state" $
                let errorState = RunError SchedulerLost
                in testUpdate Killing (Just errorState) errorState

        context "when job is in state Killing and is updated to state Running" $
            it "should change job to Killing state" $
                testUpdate Killing (Just Running) Killing

        context "when job is on scheduler and is provided no state information" $
            it "should change job to RunError SchedulerLost state" $
                testUpdate Running Nothing (RunError SchedulerLost)

        context "when job is not on scheduler and is provided no state information" $
            it "should change job to Queued LocalQueue state" $
                testUpdate (Queued LocalQueue) Nothing (Queued LocalQueue)

        context "when job is not on scheduler and is provided no state information" $
            it "should change job to RunError SchedulerLost state" $
                testUpdate (Queued LocalQueue) Nothing (Queued LocalQueue)

        context "when provided job state that is an expected transition from current state" $
            it "should change job to the provided state" $
                testUpdate (Queued LocalQueue) (Just Running) Running

        context "when provided job state is not an expected transition from current state" $
            it "should change job to RunError BadSchedulerTransition state" $
                testUpdate (Queued LocalQueue) (Just (Waiting 1)) (RunError BadSchedulerTransition)

        context "when job does not transition into a termination state" $
            it "should not change the state of any dependent jobs" $
                let oldState = effectRun $ appStateFromDepJob "dep" "job"
                    update = updateJobs (oldState ^. jobs) [("dep", Running)]
                    result = snd (ES.runEState oldState update) ^. ES.appState
                    expected = mockStateFromDepJob "dep" Running "job" (Waiting 1)
                in result `shouldBe` expected

        context "when job transitions into non-Finished termination state" $
            it "should change the state of dependent jobs to Failed DependencyFailed" $
                let oldState = effectRun $ appStateFromDepJob "dep" "job"
                    update = updateJobs (oldState ^. jobs) [("dep", Failed UserKilled)]
                    result = snd (ES.runEState oldState update) ^. ES.appState
                    expected = mockStateFromDepJob "dep" (Failed UserKilled) "job" (Failed (DependencyFailed "dep"))
                in result `shouldBe` expected

        context "when job transitions into Finished state" $
            it "should change the state of exclusively dependent job to Queued LocalQueue" $
                let oldState = effectRun $ appStateFromDepJob "dep" "job"
                    update = updateJobs (oldState ^. jobs) [("dep", Finished)]
                    result = snd (ES.runEState oldState update) ^. ES.appState
                    expected = mockStateFromDepJob "dep" (Finished) "job" (Queued LocalQueue)
                in result `shouldBe` expected

        context "when job transitions into Finished state but dependent is also waiting on another job" $
            it "should change the state of dependent job to Waiting 1" $
                let jobCreation = do
                        createJob' $ isolatedJob "dep"
                        createJob' $ isolatedJob "otherDep"
                        createJob' $ dependentJob "job" ["dep", "otherDep"]
                    oldState = effectRun jobCreation
                    update = updateJobs (oldState ^. jobs) [("dep", Finished)]
                    result = snd (ES.runEState oldState update) ^. ES.appState
                    expected = effectRun $ jobCreation >> do
                        lift $ ES.setJobState "job" (Waiting 1)
                        lift $ ES.setJobState "dep" Finished
                in result `shouldBe` expected

        context "when dependent job finishes" $
            it "should remove reverse dependency on the job on which it depended" $
                let oldState = effectRun $ appStateFromDepJob "dep" "job"
                    update = updateJobs (oldState ^. jobs) [("dep", Finished), ("job", Finished)]
                    result = snd (ES.runEState oldState update) ^. ES.appState
                in lookupRevDep "dep" result `shouldSatisfy` isNothing

testState :: Spec
testState = do
    testCreateJob
    testKillJob
    testDeleteJob
    testActiveJobs
    testUpdateJobs
