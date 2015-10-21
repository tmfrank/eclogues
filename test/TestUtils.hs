{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import qualified Eclogues.Job as Job
import Eclogues.Monitoring.Cluster (Cluster, NodeResources(..))
import Eclogues.State.Types
import qualified Eclogues.State.Monad as ES
import Eclogues.State (createJob)

import Control.Lens (view, at, (^.), (.~))
import Control.Monad.State (State)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Default.Generics (def)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromMaybe, isNothing)
import Data.Metrology.Computing (Byte (Byte), Core (Core), (%>))
import Data.Metrology.SI (Second (Second), mega)
import Data.Scientific.Suspicious (Sustific, fromFloatDigits)
import Data.Text (Text)
import Data.UUID (nil)

import Test.Hspec (Expectation, shouldSatisfy)
import Test.QuickCheck (Arbitrary (arbitrary), getNonZero)

type Scheduler = ExceptT JobError (State ES.TransitionaryState) ()

data TestError = EncounteredError JobError
               | JobNotFound Job.Name
               | RevDepNotFound Job.Name
               | ExpectedError JobError
               | UnexpectedError TestError
                 deriving (Show)

type EitherError a = Either TestError a

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither x = maybe (Left x) Right

mkResources :: Sustific -> Sustific -> Sustific -> Sustific -> Job.Resources
mkResources d r c t = fromMaybe (error "invalid test resources") $
    Job.mkResources (d %> mega Byte) (r %> mega Byte) (c %> Core) (t %> Second)

halfResources, fullResources, overResources :: Job.Resources
halfResources = mkResources 5000  1024 1 1
fullResources = mkResources 10000 2048 2 1
overResources = mkResources 20000 4096 4 1

nodeResources :: Job.Resources -> NodeResources
nodeResources res = NodeResources (res ^. Job.disk) (res ^. Job.ram) (res ^. Job.cpu)

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
isolatedJob' x = Job.Spec x "/bin/echo" halfResources [] False []

isolatedJob :: Job.Name -> Job.Resources -> Job.Spec
isolatedJob x res = Job.Spec x "/bin/echo" res [] False []

dependentJob' :: Job.Name -> [Job.Name] -> Job.Spec
dependentJob' job deps = Job.dependsOn .~ deps $ isolatedJob' job

dependentJob :: Job.Name -> [Job.Name] -> Job.Resources -> Job.Spec
dependentJob job deps res = Job.dependsOn .~ deps $ isolatedJob job res

getJob :: Job.Name -> AppState -> Maybe Job.Status
getJob jName aState = aState ^. jobs ^. at jName

noJob :: Job.Name -> EitherError AppState -> EitherError Bool
noJob jName = fmap (isNothing . getJob jName)

jobInStage :: Job.Name -> Job.Stage -> EitherError AppState -> EitherError Bool
jobInStage jName jState result = inState <$> (eitherGetJob jName =<< result)
    where eitherGetJob n = maybeToEither (JobNotFound n) . getJob n
          inState = (== jState) . view Job.stage

getRevDep :: Job.Name -> AppState -> Maybe [Job.Name]
getRevDep jName aState = aState ^. revDeps ^. at jName

jobWithRevDep :: Job.Name -> [Job.Name] -> EitherError AppState -> EitherError Bool
jobWithRevDep jName jRevDeps result = (== jRevDeps) <$> (eitherGetRevDep jName =<< result)
    where eitherGetRevDep n = maybeToEither (RevDepNotFound n) . getRevDep n

noRevDep :: Job.Name -> EitherError AppState -> EitherError Bool
noRevDep jName result = isNothing . getRevDep jName <$> result

producedError :: JobError -> EitherError AppState -> EitherError Bool
producedError jError (Left e) = case e of
    EncounteredError ex -> Right (ex == jError)
    ex                  -> Left (UnexpectedError ex)
producedError jError (Right _) = Left (ExpectedError jError)

satisfiability :: Job.Name -> Job.Satisfiability -> EitherError AppState -> EitherError Bool
satisfiability jName jSatis aState = do
    state <- aState
    let status = HM.lookup jName $ state ^. jobs
    pure $ maybe False ((== jSatis) . view Job.satis) status

forceName :: Text -> Job.Name
forceName jName = fromMaybe (error $ "invalid test name " ++ show jName) $ Job.mkName jName

instance Arbitrary Job.Resources where
    arbitrary = mk <$> v (mega Byte) <*> v (mega Byte) <*> v Core <*> v Second
      where
        v t = (%> t) . dblToSus . pos . getNonZero <$> arbitrary
        pos :: Double -> Double
        pos = (+ 1) . abs
        dblToSus :: Double -> Sustific
        dblToSus = fromFloatDigits
        mk d r c t = fromMaybe (error "arb resources failed somehow") $ Job.mkResources d r c t
