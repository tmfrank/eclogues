{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module StateSpec where

import Eclogues.API (JobError(..))
import Eclogues.Job (
    Stage (..), QueueStage (..), RunErrorReason (..), FailureReason (..))
import qualified Eclogues.Job as Job
import Eclogues.State (createJob, killJob, deleteJob, updateJobStates)
import qualified Eclogues.State.Monad as ES
import Eclogues.State.Types
import Units

import Control.Lens (view, at, (^.), (.~))
import qualified Control.Monad.State as St
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Default.Generics (def)
import Data.Maybe (isNothing, fromMaybe)
import Data.Text (Text)
import Data.UUID (nil)

import Test.Hspec

type Scheduler = ExceptT JobError (St.State ES.TransitionaryState) ()

data TestError = EncounteredError JobError
               | JobNotFound Job.Name
               | RevDepNotFound Job.Name
               | ExpectedError JobError
               | UnexpectedError TestError
                 deriving (Show)

type EitherError a = Either TestError a

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a = maybe (Left a) Right

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

createJob' :: Job.Spec -> Scheduler
createJob' = createJob nil Nothing

forceName :: Text -> Job.Name
forceName name = fromMaybe (error $ "invalid test name " ++ show name) $ Job.mkName name

job :: Job.Name
job = forceName "job"

dep :: Job.Name
dep = forceName "dep"

isolatedJob :: Job.Name -> Job.Spec
isolatedJob name = Job.Spec name "/bin/echo" res [] False []
  where
    res = Job.Resources (mega byte 10) (mebi byte 10) (core 0.1) (second 5)

dependentJob :: Job.Name -> [Job.Name] -> Job.Spec
dependentJob jobName depNames = Job.dependsOn .~ depNames $ isolatedJob jobName

getJob :: Job.Name -> AppState -> Maybe Job.Status
getJob name aState = aState ^. jobs ^. at name

noJob :: Job.Name -> EitherError AppState -> EitherError Bool
noJob name = fmap (isNothing . getJob name)

jobInStage :: Job.Name -> Job.Stage -> EitherError AppState -> EitherError Bool
jobInStage name jStage result = return . inStage =<< eitherGetJob =<< result
    where eitherGetJob = maybeToEither (JobNotFound name) . getJob name
          inStage = (== jStage) . view Job.stage

getRevDep :: Job.Name -> AppState -> Maybe [Job.Name]
getRevDep name aState = aState ^. revDeps ^. at name

jobWithRevDep :: Job.Name -> [Job.Name] -> EitherError AppState -> EitherError Bool
jobWithRevDep name jRevDeps result = return . (== jRevDeps) =<< eitherGetRevDep =<< result
    where eitherGetRevDep = maybeToEither (RevDepNotFound name) . getRevDep name

noRevDep :: Job.Name -> EitherError AppState -> EitherError Bool
noRevDep name result = return . isNothing . getRevDep name =<< result

producedError :: JobError -> EitherError AppState -> EitherError Bool
producedError jError (Left err) = case err of
    EncounteredError e -> Right (e == jError)
    e                  -> Left (UnexpectedError e)
producedError jError (Right _) = Left (ExpectedError jError)

createJobWithDep :: Job.Stage -> Scheduler
createJobWithDep depStage = do
    createJob' $ isolatedJob dep
    ES.setJobStage dep depStage
    createJob' $ dependentJob job [dep]

testCreateJob :: Spec
testCreateJob = do
    describe "createJob" $
        it "should succeed when given a unique, valid job" $
            let result = createJob' $ isolatedJob job
            in scheduler' result `shouldHave` jobInStage job (Queued LocalQueue)

    context "when provided with dependency in Finished stage" $
        it "should be placed in the Queued stage" $
            let result = createJobWithDep Finished
            in scheduler' result `shouldHave` jobInStage job (Queued LocalQueue)

    context "when provided with dependency in an active stage" $
        it "should be waiting on one job" $
            let result = createJobWithDep Running
            in scheduler' result `shouldHave` jobInStage job (Waiting 1)

    context "when provided with a dependency" $
        it "should update dependants list for that dependency" $
            let result = do
                    createJob' $ isolatedJob dep
                    createJob' $ dependentJob job [dep]
            in scheduler' result `shouldHave` jobWithRevDep dep [job]

    context "when provided a job with a name that already exists" $
        it "should return JobNameUsed error" $
            let result = do
                    createJob' $ isolatedJob job
                    createJob' $ isolatedJob job
            in scheduler' result `shouldHave` producedError JobNameUsed

    context "when provided a dependency that doesn't exist" $
        it "should return JobMustExist error" $
            let result = createJob' $ dependentJob job [dep]
            in scheduler' result `shouldHave` producedError (JobMustExist dep)

    context "when provided a dependency that has failed" $
        it "should return JobCannotHaveFailed error" $
            let result = do
                    createJob' $ isolatedJob dep
                    lift $ ES.setJobStage dep (Failed UserKilled)
                    createJob' $ dependentJob job [dep]
            in scheduler' result `shouldHave` producedError (JobCannotHaveFailed dep)

testKillJob :: Spec
testKillJob = do
    describe "killJob" $
        it "should transition job with given name to Killing stage" $
            let result = do
                    createJob' $ isolatedJob job
                    killJob job
            in scheduler' result `shouldHave` jobInStage job Killing

    context "when provided the name of a job that doesn't exist" $
        it "should return NoSuchJob error" $
            scheduler' (killJob job) `shouldHave` producedError NoSuchJob

    context "when provided the name of a job that is in a termination stage" $
        it "should return JobMustBeTerminated error" $
            let result = do
                    createJob' $ isolatedJob job
                    lift $ ES.setJobStage job Finished
                    killJob job
            in scheduler' result `shouldHave` producedError (JobMustBeTerminated False)

testDeleteJob :: Spec
testDeleteJob = do
    it "should remove the finished job with the given name from application stage" $
        let result = do
             createJob' $ isolatedJob job
             lift $ ES.setJobStage job Finished
             deleteJob job
        in scheduler' result `shouldHave` noJob job

    context "when provided the name of a job in an active stage" $
        it "should return JobMustBeTerminated error" $
            let result = do
                    createJob' $ isolatedJob job
                    lift $ ES.setJobStage job Running
                    deleteJob job
            in scheduler' result `shouldHave` producedError (JobMustBeTerminated True)

    context "when provided the name of a job that has outstanding dependants" $
        it "should return OutstandingDependants error" $
            let result = do
                    createJobWithDep Finished
                    deleteJob dep
            in scheduler' result `shouldHave` producedError (OutstandingDependants [job])

testUpdateJobs :: Spec
testUpdateJobs = let
        updated :: Scheduler -> [(Job.Name, Job.Stage)] -> EitherError AppState
        updated m statuses = scheduler' m >>= \s -> scheduler s $ lift (updateJobStates (s ^. jobs) statuses)
    in do
        describe "updateJobStates" $
            it "should do nothing when job status hasn't changed" $
                let result = do
                        createJob' $ isolatedJob job
                        lift $ ES.setJobStage job (Queued LocalQueue)
                    statuses = [(job, Queued LocalQueue)]
                in updated result statuses `shouldHave` jobInStage job (Queued LocalQueue)

        context "when job is on scheduler and no new status information is received" $
            it "should do nothing" $
                let result = do
                        createJob' $ isolatedJob job
                        lift $ ES.setJobStage job (Queued LocalQueue)
                    statuses = [(job, Queued LocalQueue)]
            in updated result statuses `shouldHave` jobInStage job (Queued LocalQueue)

        context "when job is in stage Killing and no updated information is provided" $
            it "should change job to Failed UserKilled stage" $
                let result = do
                        createJob' $ isolatedJob job
                        lift $ ES.setJobStage job Killing
                in updated result [] `shouldHave` jobInStage job (Failed UserKilled)

        context "when job is in stage Killing and is updated to stage Finished" $
            it "should change job to Failed UserKilled stage" $
                let result = do
                        createJob' $ isolatedJob job
                        lift $ ES.setJobStage job Killing
                    statuses = [(job, Finished)]
                in updated result statuses `shouldHave` jobInStage job (Failed UserKilled)

        context "when job is in stage Killing and is updated to stage RunError SchedulerLost" $
            it "should change job to RunError SchedulerLost stage" $
                let result = do
                        createJob' $ isolatedJob job
                        lift $ ES.setJobStage job Killing
                    statuses = [(job, RunError SchedulerLost)]
                in updated result statuses `shouldHave` jobInStage job (RunError SchedulerLost)

        context "when job is in stage Killing and is updated to stage Running" $
            it "should change job to Killing stage" $
                let result = do
                        createJob' $ isolatedJob job
                        lift $ ES.setJobStage job Killing
                    statuses = [(job, Running)]
                in updated result statuses `shouldHave` jobInStage job Killing

        context "when job is on scheduler and is provided no stage information" $
            it "should change job to RunError SchedulerLost stage" $
                let result = do
                        createJob' $ isolatedJob job
                        lift $ ES.setJobStage job Running
                in updated result [] `shouldHave` jobInStage job (RunError SchedulerLost)

        context "when job is not on scheduler and is provided no stage information" $
            it "should change job to Queued LocalQueue stage" $
                let result = do
                        createJob' $ isolatedJob job
                        lift $ ES.setJobStage job (Queued LocalQueue)
                in updated result [] `shouldHave` jobInStage job (Queued LocalQueue)

        context "when provided job stage that is an expected transition from current stage" $
            it "should change job to the provided stage" $
                let result = do
                        createJob' $ isolatedJob job
                        lift $ ES.setJobStage job $ Queued LocalQueue
                    statuses = [(job, Running)]
                in updated result statuses `shouldHave` jobInStage job Running

        context "when provided job stage is not an expected transition from current stage" $
            it "should change job to RunError BadSchedulerTransition stage" $
                let result = do
                        createJob' $ isolatedJob job
                        lift $ ES.setJobStage job Running
                    statuses = [(job, Waiting 1)]
                in updated result statuses `shouldHave` jobInStage job (RunError BadSchedulerTransition)

        context "when job does not transition into a termination stage" $
            it "should not change the stage of any dependent jobs" $
                let result = createJobWithDep Running
                    statuses = [(dep, Running)]
                in do
                    updated result statuses `shouldHave` jobInStage dep Running
                    updated result statuses `shouldHave` jobInStage job (Waiting 1)

        context "when job transitions into non-Finished termination stage" $
            it "should change the stage of dependent jobs to Failed DependencyFailed" $
                let result = createJobWithDep Running
                    statuses = [(dep, Failed UserKilled)]
                in do
                    updated result statuses `shouldHave` jobInStage dep (Failed UserKilled)
                    updated result statuses `shouldHave` jobInStage job (Failed (DependencyFailed dep))

        context "when job transitions into Finished stage" $
            it "should change the stage of exclusively dependent job to Queued LocalQueue" $
                let result = createJobWithDep Running
                    statuses = [(dep, Finished)]
                in do
                    updated result statuses `shouldHave` jobInStage dep Finished
                    updated result statuses `shouldHave` jobInStage job (Queued LocalQueue)

        context "when job transitions into Finished stage but dependent is also waiting on another job" $
            it "should change the stage of dependent job to Waiting 1" $
                let result = do
                        createJob' $ isolatedJob dep
                        createJob' $ isolatedJob otherDep
                        createJob' $ dependentJob job [dep, otherDep]
                    statuses = [(dep, Finished)]
                    otherDep = forceName "otherDep"
                in do
                    updated result statuses `shouldHave` jobInStage dep Finished
                    updated result statuses `shouldHave` jobInStage job (Waiting 1)

        context "when dependent job finishes" $ do
            it "should remove reverse dependency on the job on which it depended" $
                let result = do
                        createJob' $ isolatedJob dep
                        createJob' $ dependentJob job [dep]
                    statuses = [(dep, Finished), (job, Finished)]
                in updated result statuses `shouldHave` noRevDep dep

            it "should allow job and dependent to be deleted" $
                let start = do
                        createJob' $ isolatedJob dep
                        createJob' $ dependentJob job [dep]
                    modify state = do
                        let statuses = [(dep, Failed UserKilled)]
                        updateJobStates (state ^. jobs) statuses
                        ES.deleteJob dep
                        ES.deleteJob job
                    result = case scheduler' start of
                        Right state -> scheduler state (modify state)
                        Left x -> Left x
                in do
                    result `shouldHave` noJob dep
                    result `shouldHave` noJob job

            it "should recursively cause all dependencies to fail" $
                let result = do
                        createJob' $ isolatedJob dep
                        createJob' $ dependentJob job [dep]
                        createJob' $ dependentJob job2 [job]
                    statuses = [(dep, Failed UserKilled)]
                    job2 = forceName "job2"
                in do
                    updated result statuses `shouldHave` noRevDep dep
                    updated result statuses `shouldHave` noRevDep job
                    updated result statuses `shouldHave` jobInStage job (Failed (DependencyFailed dep))
                    updated result statuses `shouldHave` jobInStage job2 (Failed (DependencyFailed job))

testState :: Spec
testState = do
    testCreateJob
    testKillJob
    testDeleteJob
    testUpdateJobs
