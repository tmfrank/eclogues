{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Shannon Pace <space@swin.edu.au>
Stability   : unstable
Portability : portable

Types representing machines/cluster and functions for determing
satisfiability of jobs.
-}

module Eclogues.Monitoring.Cluster where

import Eclogues.Job (
      Resources, Satisfiability(..), Spec
    , UnsatisfiabilityReason(..)
    , resources, cpu, disk, ram )
import Units (Value, Core, MiB, MB)

import Control.Lens (view, (^.))
import Control.Lens.TH (makeClassy)
import Data.Bool (bool)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap

data MResources = MResources { _approxRam :: Value Double MiB
                             , _approxCpu :: Value Double Core
                             , _approxDisk :: Value Double MB }

$(makeClassy ''MResources)

type Cluster = HashMap String MResources

verifyResources :: Cluster -> Resources -> Bool
verifyResources cluster jobRes = not . null $ HashMap.filter sufficientRes cluster
    where
        sufficientRes mRes = suffCpu mRes && suffRam mRes && suffDisk mRes
        suffCpu mRes = mRes ^. approxCpu >= jobRes ^. cpu
        suffRam mRes = mRes ^. approxRam >= jobRes ^. ram
        suffDisk mRes = mRes ^. approxDisk >= jobRes ^. disk

satisfiability :: Cluster -> Spec -> Satisfiability
satisfiability cluster = satisfiability' . verifyResources cluster . view resources
    where satisfiability' = bool (Unsatisfiable InsufficientResources) Satisfiable
