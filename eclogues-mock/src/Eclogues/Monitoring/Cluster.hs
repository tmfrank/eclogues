{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Shannon Pace <space@swin.edu.au>
Stability   : unstable
Portability : portable

Functionality for determining the satisfiability of jobs.
-}

module Eclogues.Monitoring.Cluster (
    -- * Types
      Cluster, NodeResources (..)
    -- * Satisfiability updates
    , updateSatisfiabilities
    -- * Satisfiability checking
    , stagelessSatisfy
    ) where

import qualified Eclogues.Job as Job
import qualified Eclogues.State.Monad as ES
import Eclogues.State.Monad (TS)
import Eclogues.State.Types (AppState, jobs)

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Lens.TH (makeClassy)
import Control.Monad (when)
import Data.Bool (bool)
import Data.Foldable (traverse_)
import qualified Data.HashMap.Lazy as HashMap
import Data.Graph (Graph, Vertex, graphFromEdges, topSort)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (catMaybes, fromMaybe)
import Data.Metrology.Computing (Data, Parallelism)

data NodeResources = NodeResources { _disk :: Data
                                   , _ram  :: Data
                                   , _cpu  :: Parallelism }

$(makeClassy ''NodeResources)

type Cluster = [NodeResources]

type Node = (Job.Status, Job.Name, [Job.Name])

-- | Return whether the given resources can be accommodated within the cluster.
resourcesAvailable :: Cluster -> Job.Resources -> Bool
resourcesAvailable cluster jobRes = not . null $ filter sufficientRes cluster
    where
        sufficientRes nodeRes = suffCpu nodeRes && suffRam nodeRes && suffDisk nodeRes
        suffCpu nodeRes = nodeRes ^. cpu >= jobRes ^. Job.cpu
        suffRam nodeRes = nodeRes ^. ram >= jobRes ^. Job.ram
        suffDisk nodeRes = nodeRes ^. disk >= jobRes ^. Job.disk

-- | Return the satisfiability of a job as can be determined using cluster resources.
specSatisfy :: Job.Spec -> Cluster -> Maybe Job.Satisfiability
specSatisfy spec cluster = satisfy $ resourcesAvailable cluster (spec ^. Job.resources)
    where satisfy = bool (Just $ Job.Unsatisfiable Job.InsufficientResources) Nothing

-- | Return satisfiability of the given job status as can be determined by its state.
stageSatisfy :: Job.Status -> Maybe Job.Satisfiability
stageSatisfy status = bool (Just Job.Satisfiable) Nothing $ Job.isPendingStage (status ^. Job.stage)

-- | Return satisfiability of a hypothetical job which has the named dependencies. Returns
--   Nothing if the satisfiability of the hypothetical job cannot be determined from the
--   dependencies alone.
depSatisfy :: TS m => [Job.Name] -> m (Maybe Job.Satisfiability)
depSatisfy names = do
    let collectSatis = HM.fromList . fmap (\x -> (x ^. Job.name, x ^. Job.satis))
    satisfiabilities <- (collectSatis . catMaybes) <$> traverse ES.getJob names
    let orNothing x = bool (Just x) Nothing . HM.null
        depsFailed :: Maybe Job.Satisfiability
        depsFailed = orNothing (Job.Unsatisfiable $ badDeps failed) failed
            where
                badDeps = Job.DependenciesUnsatisfiable . HM.keys
                failed = flip HM.filter satisfiabilities $ \case
                    (Job.Unsatisfiable _) -> True
                    _                     -> False
        depUnknown :: Maybe Job.Satisfiability
        depUnknown = orNothing Job.SatisfiabilityUnknown unknown
            where
                unknown = flip HM.filter satisfiabilities $ \case
                    Job.SatisfiabilityUnknown -> True
                    _                         -> False
    pure $ depsFailed <|> depUnknown

-- | Return the satisfiability of the given job specification if this can be determined
--   from the state of the cluster and the job's dependencies.
stagelessSatisfy :: TS m => Maybe Cluster -> Job.Spec -> m Job.Satisfiability
stagelessSatisfy clusterM spec = do
    depSatisfy' <- depSatisfy $ spec ^. Job.dependsOn
    pure $ case clusterM of
        Just cluster -> fromMaybe Job.Satisfiable knownSatis
            where knownSatis = specSatisfy spec cluster <|> depSatisfy'
        Nothing      -> fromMaybe Job.SatisfiabilityUnknown depSatisfy'

-- | Update the satisfiability of a job based on its state, dependencies or
--   required resources (with respect to cluster state).
updateSatisfy :: (TS m) => Maybe Cluster -> Job.Status -> m ()
updateSatisfy clusterM status = do
    depSatisfy' <- depSatisfy (status ^. Job.dependsOn)
    let stageSatisfy' = stageSatisfy status
        satis = case clusterM of
            Just cluster -> fromMaybe Job.Satisfiable knownSatis
                where
                    knownSatis = stageSatisfy' <|> specSatisfy' <|> depSatisfy'
                    specSatisfy' = specSatisfy (status ^. Job.spec) cluster
            Nothing      -> fromMaybe Job.SatisfiabilityUnknown knownSatis
                where knownSatis = stageSatisfy' <|> depSatisfy'
    when (status ^. Job.satis /= satis) $
        ES.setJobSatis (status ^. Job.name) satis

-- | Update the satisfiability of all jobs with respect to the cluster, their state and
--   their dependencies.
updateSatisfiabilities :: (TS m) => Maybe Cluster -> AppState -> m ()
updateSatisfiabilities cluster aState = traverse_ (updateSatisfy cluster) =<< depOrdered
    where depOrdered = orderByDep $ HashMap.elems (aState ^. jobs)

-- | Order jobs with respect to dependencies.
orderByDep :: (TS m) => [Job.Status] -> m [Job.Status]
orderByDep jobStatuses = graph jobStatuses >>= \(x, y) -> pure $ fmap y (topSort x)
    where
        graph :: (TS m) => [Job.Status] -> m (Graph, Vertex -> Job.Status)
        graph statuses = do
            let nodes = traverse node statuses
            (graph', gLookup, _) <- graphFromEdges <$> nodes
            let gLookup' = (\(r, _, _) -> r) . gLookup
            pure (graph', gLookup')
        node :: (TS m) => Job.Status -> m Node
        node status = do
            dependents <- ES.getDependents $ status ^. Job.name
            pure (status, status ^. Job.name, dependents)
