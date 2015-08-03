{-# LANGUAGE OverloadedStrings #-}

module StateSpec where

import Eclogues.JobSpec
import Eclogues.State (createJob, killJob, deleteJob, updateJobs)
import Eclogues.State.Types
import qualified Eclogues.State.Monad as ES
import Eclogues.API (JobError(..))
import Units

import Test.Hspec
import Data.Default.Generics (def)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Either (isRight, isLeft)
import Data.UUID (nil)
import Control.Lens ((^.), (.~), at)
import Data.Maybe (isNothing)

type Scheduler = ExceptT JobError ES.EState ()
type Result = (Either JobError (), AppState)

defAppState :: AppState
defAppState = def

scheduler' :: Scheduler -> Result
scheduler' = scheduler defAppState

scheduler :: AppState -> Scheduler -> Result
scheduler aState m = (x, y ^. appState)
    where (x, y) = ES.runEState aState . runExceptT $ m

createJob' :: JobSpec -> Scheduler
createJob' = createJob nil

isolatedJob :: Name -> JobSpec
isolatedJob x = JobSpec x "/bin/echo" res [] False []
    where res = Resources (mega byte 10) (mebi byte 10) (core 0.1) (second 5)

dependentJob :: Name -> [Name] -> JobSpec
dependentJob job deps = dependsOn .~ deps $ isolatedJob job

shouldHave :: Result -> (Result -> Bool) -> Expectation
shouldHave result f = (f result) `shouldBe` True

getJob :: Name -> Result -> Maybe JobStatus
getJob jName result = (snd result) ^. appState ^. jobs ^. at jName

-- TODO: Address relationship between failure in Result and job existence.
noJob :: Name -> (Result -> Bool)
noJob jName = validator
    where validator result
            | (isLeft . fst) result = False
            | otherwise = isNothing $ getJob jName result

getRevDep :: Name -> Result -> Maybe [Name]
getRevDep jName result = (snd result) ^. appState ^. revDeps ^. at jName

-- TODO: Address relationship between failure in Result and reverse dependency existence.
noRevDep :: Name -> (Result -> Bool)
noRevDep jName = validator
    where validator result
            | (isLeft . fst) result = False
            | otherwise = isNothing $ getRevDep jName result

jobInState :: Name -> JobState -> (Result -> Bool)
jobInState jName jState = validator
    where validator result
            | (isLeft . fst) result = False
            | otherwise = maybe False inState $ getJob jName result
          inState = (== jState) . (flip (^.) jobState)

jobWithRevDep :: Name -> [Name] -> (Result -> Bool)
jobWithRevDep jName jRevDeps = validator
    where validator result
            | (isLeft . fst) result = False
            | otherwise = maybe False (== jRevDeps) $ getRevDep jName result

producedError :: JobError -> (Result -> Bool)
producedError jError = (either (== jError) (const False)) . fst

notProducedError :: (Result -> Bool)
notProducedError = isRight . fst

testCreateJob :: Spec
testCreateJob = do
    describe "createJob" $
        it "should succeed when given a unique, valid job" $
            let result = createJob' $ isolatedJob "job"
            in (scheduler' result) `shouldHave` (jobInState "job" $ Queued LocalQueue)

    context "when provided with dependency in Finished state" $
        it "should be placed in the Queued state" $
            let result = do
                    createJob' $ isolatedJob "dep"
                    lift $ ES.setJobState "dep" Finished
                    createJob' $ dependentJob "job" ["dep"]
            in (scheduler' result) `shouldHave` (jobInState "job" $ Queued LocalQueue)

    context "when provided with dependency in an active state" $
        it "should be waiting on one job" $
            let result = do
                    createJob' $ isolatedJob "dep"
                    lift $ ES.setJobState "dep" Running
                    createJob' $ dependentJob "job" ["dep"]
            in (scheduler' result) `shouldHave` (jobInState "job" $ Waiting 1)

    context "when provided with a dependency" $
        it "should update dependants list for that dependency" $
            let result = do
                    createJob' $ isolatedJob "dep"
                    createJob' $ dependentJob "job" ["dep"]
            in (scheduler' result) `shouldHave` (jobWithRevDep "dep" ["job"])

    context "when provided a job with a name that already exists" $
        it "should return JobNameUsed error" $
            let result = do
                    createJob' $ isolatedJob "job"
                    createJob' $ isolatedJob "job"
            in (scheduler' result) `shouldHave` (producedError JobNameUsed)

    context "when provided a dependency that doesn't exist" $
        it "should return JobMustExist error" $
            let result = createJob' $ dependentJob "job" ["dep"]
            in (scheduler' result) `shouldHave` (producedError $ JobMustExist "dep")

    context "when provided a dependency that has failed" $
        it "should return JobCannotHaveFailed error" $
            let result = do
                    createJob' $ isolatedJob "dep"
                    lift $ ES.setJobState "dep" (Failed UserKilled)
                    createJob' $ dependentJob "job" ["dep"]
            in (scheduler' result) `shouldHave` (producedError $ JobCannotHaveFailed "dep")

testKillJob :: Spec
testKillJob = do
    describe "killJob" $
        it "should transition job with given name to Killing state" $
            let result = do
                    createJob' $ isolatedJob "job"
                    killJob "job"
            in (scheduler' result) `shouldHave` (jobInState "job" Killing)

    context "when provided the name of a job that doesn't exist" $
        it "should return NoSuchJob error" $
            (scheduler' (killJob "job")) `shouldHave` (producedError NoSuchJob)

    context "when provided the name of a job that is in a termination state" $
        it "should return JobMustBeTerminated error" $
            let result = do
                    createJob' $ isolatedJob "job"
                    lift $ ES.setJobState "job" Finished
                    killJob "job"
            in (scheduler' result) `shouldHave` (producedError $ JobMustBeTerminated False)

testDeleteJob :: Spec
testDeleteJob = do
    it "should remove the finished job with the given name from application state" $
        let result = do
                createJob' $ isolatedJob "job"
                lift $ ES.setJobState "job" Finished
                deleteJob "job"
        in (scheduler' result) `shouldHave` (noJob "job")

    context "when provided the name of a job in an active state" $
        it "should return JobMustBeTerminated error" $
            let result = do
                    createJob' $ isolatedJob "job"
                    lift $ ES.setJobState "job" Running
                    deleteJob "job"
            in (scheduler' result) `shouldHave` (producedError $ JobMustBeTerminated True)

    context "when provided the name of a job that has outstanding dependants" $
        it "should return OutstandingDependants error" $
            let result = do
                    createJob' $ isolatedJob "dep"
                    lift $ ES.setJobState "dep" Finished
                    createJob' $ dependentJob "job" ["dep"]
                    deleteJob "dep"
            in (scheduler' result) `shouldHave` (producedError $ OutstandingDependants ["job"])

testUpdateJobs :: Spec
testUpdateJobs = let
        updated :: Scheduler -> [(Name, JobState)] -> Result
        updated m statuses = scheduler s $ lift (updateJobs (s ^. jobs) statuses)
            where (_, s) = scheduler' m
    in do
        describe "updateJobs" $
            it "should do nothing when job status hasn't changed" $
                let result = do
                        createJob' $ isolatedJob "job"
                        lift $ ES.setJobState "job" (Queued LocalQueue)
                    statuses = [("job", Queued LocalQueue)]
                in (updated result statuses) `shouldHave` (jobInState "job" $ Queued LocalQueue)

        context "when job is on scheduler and no new status information is received" $
            it "should do nothing" $
                let result = do
                        createJob' $ isolatedJob "job"
                        lift $ ES.setJobState "job" (Queued LocalQueue)
                    statuses = [("job", Queued LocalQueue)]
            in (updated result statuses) `shouldHave` (jobInState "job" $ Queued LocalQueue)

        context "when job is in state Killing and no updated information is provided" $
            it "should change job to Failed UserKilled state" $
                let result = do
                        createJob' $ isolatedJob "job"
                        lift $ ES.setJobState "job" Killing
                in (updated result []) `shouldHave` (jobInState "job" $ Failed UserKilled)

        context "when job is in state Killing and is updated to state Finished" $
            it "should change job to Failed UserKilled state" $
                let result = do
                        createJob' $ isolatedJob "job"
                        lift $ ES.setJobState "job" Killing
                    statuses = [("job", Finished)]
                in (updated result statuses) `shouldHave` (jobInState "job" $ Failed UserKilled)

        context "when job is in state Killing and is updated to state RunError SchedulerLost" $
            it "should change job to RunError SchedulerLost state" $
                let result = do
                        createJob' $ isolatedJob "job"
                        lift $ ES.setJobState "job" Killing
                    statuses = [("job", RunError SchedulerLost)]
                in (updated result statuses) `shouldHave` (jobInState "job" $ RunError SchedulerLost)

        context "when job is in state Killing and is updated to state Running" $
            it "should change job to Killing state" $
                let result = do
                        createJob' $ isolatedJob "job"
                        lift $ ES.setJobState "job" Killing
                    statuses = [("job", Running)]
                in (updated result statuses) `shouldHave` (jobInState "job" $ Killing)

        context "when job is on scheduler and is provided no state information" $
            it "should change job to RunError SchedulerLost state" $
                let result = do
                        createJob' $ isolatedJob "job"
                        lift $ ES.setJobState "job" Running
                in (updated result []) `shouldHave` (jobInState "job" $ RunError SchedulerLost)

        context "when job is not on scheduler and is provided no state information" $
            it "should change job to Queued LocalQueue state" $
                let result = do
                        createJob' $ isolatedJob "job"
                        lift $ ES.setJobState "job" (Queued LocalQueue)
                in (updated result []) `shouldHave` (jobInState "job" $ Queued LocalQueue)

        context "when provided job state that is an expected transition from current state" $
            it "should change job to the provided state" $
                let result = do
                        createJob' $ isolatedJob "job"
                        lift $ ES.setJobState "job" $ Queued LocalQueue
                    statuses = [("job", Running)]
                in (updated result statuses) `shouldHave` (jobInState "job" Running)

        context "when provided job state is not an expected transition from current state" $
            it "should change job to RunError BadSchedulerTransition state" $
                let result = do
                        createJob' $ isolatedJob "job"
                        lift $ ES.setJobState "job" Running
                    statuses = [("job", Waiting 1)]
                in (updated result statuses) `shouldHave` (jobInState "job" $ RunError BadSchedulerTransition)

        context "when job does not transition into a termination state" $
            it "should not change the state of any dependent jobs" $
                let result = do
                        createJob' $ isolatedJob "dep"
                        lift $ ES.setJobState "dep" Running
                        createJob' $ dependentJob "job" ["dep"]
                    statuses = [("dep", Running)]
                in do
                    (updated result statuses) `shouldHave` (jobInState "dep" $ Running)
                    (updated result statuses) `shouldHave` (jobInState "job" $ Waiting 1)

        context "when job transitions into non-Finished termination state" $
            it "should change the state of dependent jobs to Failed DependencyFailed" $
                let result = do
                        createJob' $ isolatedJob "dep"
                        lift $ ES.setJobState "dep" Running
                        createJob' $ dependentJob "job" ["dep"]
                    statuses = [("dep", Failed UserKilled)]
                in do
                    (updated result statuses) `shouldHave` (jobInState "dep" $ Failed UserKilled)
                    (updated result statuses) `shouldHave` (jobInState "job" $ Failed (DependencyFailed "dep"))

        context "when job transitions into Finished state" $
            it "should change the state of exclusively dependent job to Queued LocalQueue" $
                let result = do
                        createJob' $ isolatedJob "dep"
                        lift $ ES.setJobState "dep" Running
                        createJob' $ dependentJob "job" ["dep"]
                    statuses = [("dep", Finished)]
                in do
                    (updated result statuses) `shouldHave` (jobInState "dep" Finished)
                    (updated result statuses) `shouldHave` (jobInState "job" $ Queued LocalQueue)

        context "when job transitions into Finished state but dependent is also waiting on another job" $
            it "should change the state of dependent job to Waiting 1" $
                let result = do
                        createJob' $ isolatedJob "dep"
                        createJob' $ isolatedJob "otherDep"
                        createJob' $ dependentJob "job" ["dep", "otherDep"]
                    statuses = [("dep", Finished)]
                in do
                    (updated result statuses) `shouldHave` (jobInState "dep" Finished)
                    (updated result statuses) `shouldHave` (jobInState "job" $ Waiting 1)

        context "when dependent job finishes" $
            it "should remove reverse dependency on the job on which it depended" $
                let result = do
                        createJob' $ isolatedJob "dep"
                        createJob' $ dependentJob "job" ["dep"]
                    statuses = [("dep", Finished), ("job", Finished)]
                in (updated result statuses) `shouldHave` (noRevDep "dep")

testState :: Spec
testState = do
    testCreateJob
    testKillJob
    testDeleteJob
    testUpdateJobs
