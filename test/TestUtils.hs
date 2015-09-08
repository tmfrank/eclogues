{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Shannon Pace <space@swin.edu.au>
Stability   : unstable
Portability : portable

Utilities for testing Eclogues functionality.
-}

module TestUtils where

import Eclogues.API (JobError(..))
import Eclogues.Job as Job
import Eclogues.Monitoring.Cluster (Cluster)
import Eclogues.State.Types
import qualified Eclogues.State.Monad as ES
import Eclogues.State (createJob)
import Units

import Data.Default.Generics (def)
import Control.Monad.State (State)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.UUID (nil)
import Control.Lens (view, at, (^.), (.~))
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)

import Test.Hspec (Expectation, shouldSatisfy)

type Scheduler = ExceptT JobError (State ES.TransitionaryState) ()

data TestError = EncounteredError JobError
               | JobNotFound Name
               | RevDepNotFound Name
               | ExpectedError JobError
               | UnexpectedError TestError
                 deriving (Show)

type EitherError a = Either TestError a

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither x = maybe (Left x) Right

halfResources, fullResources, overResources :: Job.Resources
halfResources = Job.Resources (mega byte 5000)  (mebi byte 1024) (core 1) (second 0)
fullResources = Job.Resources (mega byte 10000) (mebi byte 2048) (core 2) (second 0)
overResources = Job.Resources (mega byte 20000) (mebi byte 4096) (core 4) (second 0)

scheduler' :: Scheduler -> EitherError AppState
scheduler' = scheduler def

scheduler :: AppState -> Scheduler -> EitherError AppState
scheduler aState = packageResult . fmap (^. appState) . ES.runState aState . runExceptT

packageResult :: (Either JobError (), AppState) -> EitherError AppState
packageResult (Left jError, _) = Left (EncounteredError jError)
packageResult (_, effect) = Right (effect ^. appState)

shouldHave :: EitherError AppState -> (EitherError AppState -> EitherError Bool) -> Expectation
shouldHave result f = result `shouldSatisfy` either (const False) id . f

createJob' :: Job.Spec -> Scheduler
createJob' = createJob nil Nothing

createWithCluster :: Cluster -> Job.Spec -> Scheduler
createWithCluster cluster = createJob nil (Just cluster)

isolatedJob' :: Job.Name -> Job.Spec
isolatedJob' x = Spec x "/bin/echo" halfResources [] False []

isolatedJob :: Job.Name -> Job.Resources -> Job.Spec
isolatedJob x res = Job.Spec x "/bin/echo" res [] False []

dependentJob' :: Job.Name -> [Job.Name] -> Job.Spec
dependentJob' job deps = dependsOn .~ deps $ isolatedJob' job

dependentJob :: Job.Name -> [Job.Name] -> Job.Resources -> Job.Spec
dependentJob job deps res = dependsOn .~ deps $ isolatedJob job res

getJob :: Job.Name -> AppState -> Maybe Status
getJob jName aState = aState ^. jobs ^. at jName

noJob :: Job.Name -> EitherError AppState -> EitherError Bool
noJob jName = fmap (isNothing . getJob jName)

jobInStage :: Job.Name -> Job.Stage -> EitherError AppState -> EitherError Bool
jobInStage jName jState result = return . inState =<< eitherGetJob jName =<< result
    where eitherGetJob n = maybeToEither (JobNotFound n) . getJob n
          inState = (== jState) . view stage

getRevDep :: Job.Name -> AppState -> Maybe [Name]
getRevDep jName aState = aState ^. revDeps ^. at jName

jobWithRevDep :: Job.Name -> [Job.Name] -> EitherError AppState -> EitherError Bool
jobWithRevDep jName jRevDeps result = return . (== jRevDeps) =<< eitherGetRevDep jName =<< result
    where eitherGetRevDep n = maybeToEither (RevDepNotFound n) . getRevDep n

noRevDep :: Job.Name -> EitherError AppState -> EitherError Bool
noRevDep jName result = return . isNothing . getRevDep jName =<< result

producedError :: JobError -> EitherError AppState -> EitherError Bool
producedError jError (Left e) = case e of
    EncounteredError ex -> Right (ex == jError)
    ex                  -> Left (UnexpectedError ex)
producedError jError (Right _) = Left (ExpectedError jError)

forceName :: Text -> Job.Name
forceName jName = fromMaybe (error $ "invalid test name " ++ show jName) $ mkName jName
