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
                                     Cluster, NodeResources(..),
                                   -- * Satisfiability updates
                                     updateSatisfiabilities, updateSatisfy, allSatisfyUnknown,
                                   -- * Satisfiability checking
                                     specSatisfy, depSatisfy, jobDepSatisfy
                                   ) where

import qualified Eclogues.Job as Job
import Eclogues.Job (Name, Resources, Spec, Status, Satisfiability(..), UnsatisfiableReason(..))
import qualified Eclogues.State.Monad as ES
import Eclogues.State.Monad (TS)
import Eclogues.State.Types (AppState, jobs)
import Units as U

import Control.Lens (view, (^.))
import Control.Lens.TH (makeClassy)
import Data.Bool (bool)
import qualified Data.HashMap.Lazy as HashMap
import Data.Graph (Graph, Vertex, graphFromEdges, topSort)
import Data.Maybe (catMaybes, fromMaybe)
import Control.Monad (void, when)

data NodeResources = NodeResources { _disk :: U.Value Double U.MB
                                   , _ram  :: U.Value Double U.MiB
                                   , _cpu  :: U.Value Double U.Core }

$(makeClassy ''NodeResources)

type Cluster = [NodeResources]

type Node = (Status, Name, [Name])

data Satisfiabilities = Satisfiabilities { _satisfiable   :: [Status]
                                         , _unsatisfiable :: [Status]
                                         , _unknown       :: [Status] }

$(makeClassy ''Satisfiabilities)

-- | Return whether the given resources can be accommodated within the cluster.
verifyResources :: Cluster -> Resources -> Bool
verifyResources cluster jobRes = not . null $ filter sufficientRes cluster
    where
        sufficientRes nodeRes = suffCpu nodeRes && suffRam nodeRes && suffDisk nodeRes
        suffCpu nodeRes = nodeRes ^. cpu >= jobRes ^. Job.cpu
        suffRam nodeRes = nodeRes ^. ram >= jobRes ^. Job.ram
        suffDisk nodeRes = nodeRes ^. disk >= jobRes ^. Job.disk

-- | Return the satisfiability of a job with respect to cluster resources.
specSatisfy :: Cluster -> Spec -> Satisfiability
specSatisfy cluster = satisfy . verifyResources cluster . view Job.resources
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
        where badDeps = DependenciesUnsatisfiable . map (view Job.name . view Job.spec)

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

-- | Determine the satisfiability of a job with respect to its dependencies.
depSatisfy :: (TS m) => [Name] -> m (Maybe Satisfiability)
depSatisfy deps = fmap (minSatisfy . groupSatisfy) $ catMaybes <$> mapM ES.getJob deps

-- | Return job satisfiability factoring in the resources it requires and the
--   satisfiability of its dependencies.
jobDepSatisfy :: (TS m) => Status -> Cluster -> m Satisfiability
jobDepSatisfy status cluster = case specSatisfy cluster $ status ^. Job.spec of
    Job.Unsatisfiable reason -> pure $ Job.Unsatisfiable reason
    x -> pure . fromMaybe x =<< depSatisfy (status ^. Job.dependsOn)

-- | Update the satisfiability of a single job with respect to the cluster, its state and
--   its dependencies.
updateSatisfy :: (TS m) => Cluster -> Status -> m ()
updateSatisfy cluster status = do
    satis <- maybe (jobDepSatisfy status cluster) pure $ stateSatisfy status
    when (status ^. Job.satis /= satis) $
        ES.setJobSatis (status ^. Job.name) satis

-- | Update the satisfiability of all jobs with respect to the cluster, their state and
--   their dependencies.
updateSatisfiabilities :: (TS m) => Cluster -> AppState -> m ()
updateSatisfiabilities cluster aState = mapM_ (updateSatisfy cluster) =<< depOrdered
    where depOrdered = orderByDep $ HashMap.elems (aState ^. jobs)

-- | Set the satisfiability of all jobs that are not running or completed to unknown.
allSatisfyUnknown :: (TS m) => AppState -> m ()
allSatisfyUnknown aState = void $ HashMap.traverseWithKey stateOrUnknown (aState ^. jobs)
    where
        stateOrUnknown k v = maybe (setUnknown k) (ES.setJobSatis k) $ stateSatisfy v
        setUnknown k = ES.setJobSatis k SatisfiabilityUnknown

-- | Order jobs with respect to dependencies.
orderByDep :: (TS m) => [Job.Status] -> m [Job.Status]
orderByDep jobStatuses = graph jobStatuses >>= \(x, y) -> pure $ map y (topSort x)
    where
        graph :: (TS m) => [Status] -> m (Graph, Vertex -> Status)
        graph statuses = do
            let nodes = mapM node statuses
            (graph', gLookup, _) <- graphFromEdges <$> nodes
            let gLookup' = (\(r, _, _) -> r) . gLookup
            pure (graph', gLookup')
        node :: (TS m) => Job.Status -> m Node
        node status = do
            dependents <- ES.getDependents $ status ^. Job.name
            pure (status, status ^. Job.name, dependents)
