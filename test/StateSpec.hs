{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module StateSpec where

import Eclogues.JobSpec
import Eclogues.State (createJob, killJob, deleteJob, updateJobs)
import Eclogues.State.Types
import qualified Eclogues.State.Monad as ES
import Eclogues.API (JobError(..))
import Units

import Test.Hspec
import Data.Default.Generics (def)
import Control.Monad.State (State)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Either (isRight, isLeft)
import Data.UUID (nil)
import Control.Lens (view, at, (^.), (.~))
import Data.Maybe (isNothing)

type Scheduler = ExceptT JobError (State ES.TransitionaryState) ()

data TestError = EncounteredError JobError
               | JobNotFound Name
               | RevDepNotFound Name
               | ExpectedError JobError
               | UnexpectedError TestError
                 deriving (Show)

type EitherError a = Either TestError a

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither def = maybe (Left def) Right

defAppState :: AppState
defAppState = def

scheduler' :: Scheduler -> EitherError AppState
scheduler' = scheduler defAppState

scheduler :: AppState -> Scheduler -> EitherError AppState
scheduler aState = packageResult . fmap (^. appState) . ES.runState aState . runExceptT

packageResult :: (Either JobError (), AppState) -> EitherError AppState
packageResult (Left jError, _) = Left (EncounteredError jError)
packageResult (_, effect) = Right (effect ^. appState)

shouldHave :: EitherError AppState -> (EitherError AppState -> EitherError Bool) -> Expectation
shouldHave result f = result `shouldSatisfy` either (const False) id . f

createJob' :: JobSpec -> Scheduler
createJob' = createJob nil

isolatedJob :: Name -> JobSpec
isolatedJob x = JobSpec x "/bin/echo" res [] False []
    where res = Resources (mega byte 10) (mebi byte 10) (core 0.1) (second 5)

dependentJob :: Name -> [Name] -> JobSpec
dependentJob job deps = dependsOn .~ deps $ isolatedJob job

getJob :: Name -> AppState -> Maybe JobStatus
getJob jName aState = aState ^. jobs ^. at jName

noJob :: Name -> EitherError AppState -> EitherError Bool
noJob jName = fmap (isNothing . getJob jName)

jobInState :: Name -> JobState -> EitherError AppState -> EitherError Bool
jobInState jName jState result = return . inState =<< eitherGetJob jName =<< result
    where eitherGetJob jName = maybeToEither (JobNotFound jName) . getJob jName
          inState = (== jState) . view jobState

getRevDep :: Name -> AppState -> Maybe [Name]
getRevDep jName aState = aState ^. revDeps ^. at jName

jobWithRevDep :: Name -> [Name] -> EitherError AppState -> EitherError Bool
jobWithRevDep jName jRevDeps result = return . (== jRevDeps) =<< eitherGetRevDep jName =<< result
    where eitherGetRevDep jName = maybeToEither (RevDepNotFound jName) . getRevDep jName

noRevDep :: Name -> EitherError AppState -> EitherError Bool
noRevDep jName result = return . isNothing . getRevDep jName =<< result

producedError :: JobError -> EitherError AppState -> EitherError Bool
producedError jError (Left e) = case e of
    EncounteredError e -> Right (e == jError)
    e                  -> Left (UnexpectedError e)
producedError jError (Right e) = Left (ExpectedError jError)

createJobWithDep depState = do
    createJob' $ isolatedJob "dep"
    ES.setJobState "dep" depState
    createJob' $ dependentJob "job" ["dep"]

testCreateJob :: Spec
testCreateJob = do
    describe "createJob" $
        it "should succeed when given a unique, valid job" $
            let result = createJob' $ isolatedJob "job"
            in scheduler' result `shouldHave` jobInState "job" (Queued LocalQueue)

    context "when provided with dependency in Finished state" $
        it "should be placed in the Queued state" $
            let result = createJobWithDep Finished
            in scheduler' result `shouldHave` jobInState "job" (Queued LocalQueue)

    context "when provided with dependency in an active state" $
        it "should be waiting on one job" $
            let result = createJobWithDep Running
            in scheduler' result `shouldHave` jobInState "job" (Waiting 1)

    context "when provided with a dependency" $
        it "should update dependants list for that dependency" $
            let result = do
                    createJob' $ isolatedJob "dep"
                    createJob' $ dependentJob "job" ["dep"]
            in scheduler' result `shouldHave` jobWithRevDep "dep" ["job"]

    context "when provided a job with a name that already exists" $
        it "should return JobNameUsed error" $
            let result = do
                    createJob' $ isolatedJob "job"
                    createJob' $ isolatedJob "job"
            in scheduler' result `shouldHave` producedError JobNameUsed

    context "when provided a dependency that doesn't exist" $
        it "should return JobMustExist error" $
            let result = createJob' $ dependentJob "job" ["dep"]
            in scheduler' result `shouldHave` producedError (JobMustExist "dep")

    context "when provided a dependency that has failed" $
        it "should return JobCannotHaveFailed error" $
            let result = do
                    createJob' $ isolatedJob "dep"
                    lift $ ES.setJobState "dep" (Failed UserKilled)
                    createJob' $ dependentJob "job" ["dep"]
            in scheduler' result `shouldHave` producedError (JobCannotHaveFailed "dep")

testKillJob :: Spec
testKillJob = do
    describe "killJob" $
        it "should transition job with given name to Killing state" $
            let result = do
                    createJob' $ isolatedJob "job"
                    killJob "job"
            in scheduler' result `shouldHave` jobInState "job" Killing

    context "when provided the name of a job that doesn't exist" $
        it "should return NoSuchJob error" $
            scheduler' (killJob "job") `shouldHave` producedError NoSuchJob

    context "when provided the name of a job that is in a termination state" $
        it "should return JobMustBeTerminated error" $
            let result = do
                    createJob' $ isolatedJob "job"
                    lift $ ES.setJobState "job" Finished
                    killJob "job"
            in scheduler' result `shouldHave` producedError (JobMustBeTerminated False)

testDeleteJob :: Spec
testDeleteJob = do
    it "should remove the finished job with the given name from application state" $
        let result = do
             createJob' $ isolatedJob "job"
             lift $ ES.setJobState "job" Finished
             deleteJob "job"
        in scheduler' result `shouldHave` noJob "job"

    context "when provided the name of a job in an active state" $
        it "should return JobMustBeTerminated error" $
            let result = do
                    createJob' $ isolatedJob "job"
                    lift $ ES.setJobState "job" Running
                    deleteJob "job"
            in scheduler' result `shouldHave` producedError (JobMustBeTerminated True)

    context "when provided the name of a job that has outstanding dependants" $
        it "should return OutstandingDependants error" $
            let result = do
                    createJobWithDep Finished
                    deleteJob "dep"
            in scheduler' result `shouldHave` producedError (OutstandingDependants ["job"])

testUpdateJobs :: Spec
testUpdateJobs = let
        updated :: Scheduler -> [(Name, JobState)] -> EitherError AppState
        updated m statuses = scheduler' m >>= \s -> scheduler s $ lift (updateJobs (s ^. jobs) statuses)
    in do
        describe "updateJobs" $
            it "should do nothing when job status hasn't changed" $
                let result = do
                        createJob' $ isolatedJob "job"
                        lift $ ES.setJobState "job" (Queued LocalQueue)
                    statuses = [("job", Queued LocalQueue)]
                in updated result statuses `shouldHave` jobInState "job" (Queued LocalQueue)

        context "when job is on scheduler and no new status information is received" $
            it "should do nothing" $
                let result = do
                        createJob' $ isolatedJob "job"
                        lift $ ES.setJobState "job" (Queued LocalQueue)
                    statuses = [("job", Queued LocalQueue)]
            in updated result statuses `shouldHave` jobInState "job" (Queued LocalQueue)

        context "when job is in state Killing and no updated information is provided" $
            it "should change job to Failed UserKilled state" $
                let result = do
                        createJob' $ isolatedJob "job"
                        lift $ ES.setJobState "job" Killing
                in updated result [] `shouldHave` jobInState "job" (Failed UserKilled)

        context "when job is in state Killing and is updated to state Finished" $
            it "should change job to Failed UserKilled state" $
                let result = do
                        createJob' $ isolatedJob "job"
                        lift $ ES.setJobState "job" Killing
                    statuses = [("job", Finished)]
                in updated result statuses `shouldHave` jobInState "job" (Failed UserKilled)

        context "when job is in state Killing and is updated to state RunError SchedulerLost" $
            it "should change job to RunError SchedulerLost state" $
                let result = do
                        createJob' $ isolatedJob "job"
                        lift $ ES.setJobState "job" Killing
                    statuses = [("job", RunError SchedulerLost)]
                in updated result statuses `shouldHave` jobInState "job" (RunError SchedulerLost)

        context "when job is in state Killing and is updated to state Running" $
            it "should change job to Killing state" $
                let result = do
                        createJob' $ isolatedJob "job"
                        lift $ ES.setJobState "job" Killing
                    statuses = [("job", Running)]
                in updated result statuses `shouldHave` jobInState "job" Killing

        context "when job is on scheduler and is provided no state information" $
            it "should change job to RunError SchedulerLost state" $
                let result = do
                        createJob' $ isolatedJob "job"
                        lift $ ES.setJobState "job" Running
                in updated result [] `shouldHave` jobInState "job" (RunError SchedulerLost)

        context "when job is not on scheduler and is provided no state information" $
            it "should change job to Queued LocalQueue state" $
                let result = do
                        createJob' $ isolatedJob "job"
                        lift $ ES.setJobState "job" (Queued LocalQueue)
                in updated result [] `shouldHave` jobInState "job" (Queued LocalQueue)

        context "when provided job state that is an expected transition from current state" $
            it "should change job to the provided state" $
                let result = do
                        createJob' $ isolatedJob "job"
                        lift $ ES.setJobState "job" $ Queued LocalQueue
                    statuses = [("job", Running)]
                in updated result statuses `shouldHave` jobInState "job" Running

        context "when provided job state is not an expected transition from current state" $
            it "should change job to RunError BadSchedulerTransition state" $
                let result = do
                        createJob' $ isolatedJob "job"
                        lift $ ES.setJobState "job" Running
                    statuses = [("job", Waiting 1)]
                in updated result statuses `shouldHave` jobInState "job" (RunError BadSchedulerTransition)

        context "when job does not transition into a termination state" $
            it "should not change the state of any dependent jobs" $
                let result = createJobWithDep Running
                    statuses = [("dep", Running)]
                in do
                    updated result statuses `shouldHave` jobInState "dep" Running
                    updated result statuses `shouldHave` jobInState "job" (Waiting 1)

        context "when job transitions into non-Finished termination state" $
            it "should change the state of dependent jobs to Failed DependencyFailed" $
                let result = createJobWithDep Running
                    statuses = [("dep", Failed UserKilled)]
                in do
                    updated result statuses `shouldHave` jobInState "dep" (Failed UserKilled)
                    updated result statuses `shouldHave` jobInState "job" (Failed (DependencyFailed "dep"))

        context "when job transitions into Finished state" $
            it "should change the state of exclusively dependent job to Queued LocalQueue" $
                let result = createJobWithDep Running
                    statuses = [("dep", Finished)]
                in do
                    updated result statuses `shouldHave` jobInState "dep" Finished
                    updated result statuses `shouldHave` jobInState "job" (Queued LocalQueue)

        context "when job transitions into Finished state but dependent is also waiting on another job" $
            it "should change the state of dependent job to Waiting 1" $
                let result = do
                        createJob' $ isolatedJob "dep"
                        createJob' $ isolatedJob "otherDep"
                        createJob' $ dependentJob "job" ["dep", "otherDep"]
                    statuses = [("dep", Finished)]
                in do
                    updated result statuses `shouldHave` jobInState "dep" Finished
                    updated result statuses `shouldHave` jobInState "job" (Waiting 1)

        context "when dependent job finishes" $ do
            it "should remove reverse dependency on the job on which it depended" $
                let result = do
                        createJob' $ isolatedJob "dep"
                        createJob' $ dependentJob "job" ["dep"]
                    statuses = [("dep", Finished), ("job", Finished)]
                in updated result statuses `shouldHave` noRevDep "dep"

            it "should allow job and dependent to be deleted" $
                let init = do
                        createJob' $ isolatedJob "dep"
                        createJob' $ dependentJob "job" ["dep"]
                    modify state = do
                        let statuses = [("dep", Failed UserKilled)]
                        updateJobs (state ^. jobs) statuses
                        ES.deleteJob "dep"
                        ES.deleteJob "job"
                    result = case scheduler' init of
                        Right state -> scheduler state (modify state)
                        Left x -> Left x
                in do
                    result `shouldHave` noJob "dep"
                    result `shouldHave` noJob "job"

            it "should recursively cause all dependencies to fail" $
                let result = do
                        createJob' $ isolatedJob "dep"
                        createJob' $ dependentJob "job" ["dep"]
                        createJob' $ dependentJob "job2" ["job"]
                    statuses = [("dep", Failed UserKilled)]
                in do
                    updated result statuses `shouldHave` noRevDep "dep"
                    updated result statuses `shouldHave` noRevDep "job"
                    updated result statuses `shouldHave` jobInState "job" (Failed (DependencyFailed "dep"))
                    updated result statuses `shouldHave` jobInState "job2" (Failed (DependencyFailed "job"))

testState :: Spec
testState = do
    testCreateJob
    testKillJob
    testDeleteJob
    testUpdateJobs
