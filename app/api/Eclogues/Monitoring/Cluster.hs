{-# LANGUAGE FlexibleContexts #-}
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
    , updateSatisfiabilities, allSatisfyUnknown
    -- * Satisfiability checking
    , jobDepSatisfy
    ) where

import qualified Eclogues.Job as Job
import Eclogues.Job (Name, Resources, Spec, Status, Satisfiability (..), UnsatisfiableReason (..))
import qualified Eclogues.State.Monad as ES
import Eclogues.State.Monad (TS)
import Eclogues.State.Types (AppState, jobs)

import Control.Lens ((^.), (<&>), view, itraverse_)
import Control.Lens.TH (makeClassy)
import Control.Monad (when)
import Data.Bool (bool)
import qualified Data.HashMap.Lazy as HashMap
import Data.Graph (Graph, Vertex, graphFromEdges, topSort)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Metrology.Computing (Data, Parallelism)

data NodeResources = NodeResources { _disk :: Data
                                   , _ram  :: Data
                                   , _cpu  :: Parallelism }

$(makeClassy ''NodeResources)

type Cluster = [NodeResources]

type Node = (Status, Name, [Name])

data Satisfiabilities = Satisfiabilities { _satisfiable   :: [Status]
                                         , _unsatisfiable :: [Status]
                                         , _unknown       :: [Status] }

$(makeClassy ''Satisfiabilities)

-- | Return whether the given resources can be accommodated within the cluster.
resourcesAvailable :: Cluster -> Resources -> Bool
resourcesAvailable cluster jobRes = not . null $ filter sufficientRes cluster
    where
        sufficientRes nodeRes = suffCpu nodeRes && suffRam nodeRes && suffDisk nodeRes
        suffCpu nodeRes = nodeRes ^. cpu >= jobRes ^. Job.cpu
        suffRam nodeRes = nodeRes ^. ram >= jobRes ^. Job.ram
        suffDisk nodeRes = nodeRes ^. disk >= jobRes ^. Job.disk

-- | Return the satisfiability of a job with respect to cluster resources.
specSatisfy :: Cluster -> Spec -> Satisfiability
specSatisfy cluster = satisfy . resourcesAvailable cluster . view Job.resources
    where satisfy = bool (Unsatisfiable InsufficientResources) Satisfiable

-- | Return satisfiability of the given job status as can be determined by its state.
--   Job is satisfiable if running or complete, otherwise satisfiability cannot be determined.
stateSatisfy :: Status -> Maybe Satisfiability
stateSatisfy status = bool (Just Satisfiable) Nothing $ Job.isPendingStage (status ^. Job.stage)

-- | Return the collective satisfiability of a set of satisfiable, unsatisfiable
--   and satisfiability-unknown jobs. Assumes a dependency context.
minSatisfy :: Satisfiabilities -> Maybe Satisfiability
minSatisfy sfs
    | not . null $ sfs ^. unsatisfiable = Just . Unsatisfiable $ badDeps (sfs ^. unsatisfiable)
    | not . null $ sfs ^. unknown       = Just SatisfiabilityUnknown
    | not . null $ sfs ^. satisfiable   = Just Satisfiable
    | otherwise                         = Nothing
        where badDeps = DependenciesUnsatisfiable . map (view Job.name)

-- | Return a record containing the given job statuses bucketed according to satisfiability.
groupSatisfy :: [Status] -> Satisfiabilities
groupSatisfy st = Satisfiabilities (filter satisfiable' st) (filter unsatisfiable' st) (filter unknown' st)
    where
        satisfiable', unknown', unsatisfiable' :: Status -> Bool
        satisfiable' status = status ^. Job.satis == Satisfiable
        unsatisfiable' status = case status ^. Job.satis of
            Unsatisfiable _ -> True
            _ -> False
        unknown' status = status ^. Job.satis == SatisfiabilityUnknown

-- | Determine the minimum satisfiability of a number of jobs by name.
jobsMinSatisfy :: (TS m) => [Name] -> m (Maybe Satisfiability)
jobsMinSatisfy deps = minSatisfy . groupSatisfy . catMaybes <$> mapM ES.getJob deps

-- | Return job satisfiability factoring in the resources it requires and the
--   satisfiability of its dependencies.
jobDepSatisfy :: (TS m) => Spec -> Cluster -> m Satisfiability
jobDepSatisfy spec cluster = case specSatisfy cluster spec of
    u@(Job.Unsatisfiable _) -> pure u
    x                       -> fromMaybe x <$> jobsMinSatisfy (spec ^. Job.dependsOn)

-- | Update the satisfiability of a single job with respect to the cluster, its state and
--   its dependencies.
updateSatisfy :: (TS m) => Cluster -> Status -> m ()
updateSatisfy cluster status = do
    satis <- maybe (jobDepSatisfy (status ^. Job.spec) cluster) pure $ stateSatisfy status
    when (status ^. Job.satis /= satis) $
        ES.setJobSatis (status ^. Job.name) satis

-- | Update the satisfiability of all jobs with respect to the cluster, their state and
--   their dependencies.
updateSatisfiabilities :: (TS m) => Cluster -> AppState -> m ()
updateSatisfiabilities cluster aState = mapM_ (updateSatisfy cluster) =<< depOrdered
    where depOrdered = orderByDep $ HashMap.elems (aState ^. jobs)

-- | > orUnknown = fromMaybe SatisfiabilityUnknown
orUnknown :: Maybe Satisfiability -> Satisfiability
orUnknown = fromMaybe SatisfiabilityUnknown

-- | Set the satisfiability of all jobs that are not running or completed to unknown.
allSatisfyUnknown :: (TS m) => AppState -> m ()
allSatisfyUnknown aState = itraverse_ stateOrUnknown (aState ^. jobs)
    where
        stateOrUnknown k v = ES.setJobSatis k . orUnknown $ stateSatisfy v

-- | Order jobs with respect to dependencies.
orderByDep :: (TS m) => [Job.Status] -> m [Job.Status]
orderByDep jobStatuses = graph jobStatuses <&> \(x, y) -> map y (topSort x)
    where
        graph :: (TS m) => [Status] -> m (Graph, Vertex -> Status)
        graph statuses = do
            let nodes = mapM node statuses
            (graph', gLookup, _) <- graphFromEdges <$> nodes
            let gLookup' = (\(r, _, _) -> r) . gLookup
            pure (graph', gLookup')
        node :: (TS m) => Job.Status -> m Node
        node status = do
            let name = status ^. Job.name
            dependents <- ES.getDependents name
            pure (status, name, dependents)
