{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Shannon Pace <space@swin.edu.au>
Stability   : unstable
Portability : portable

Tests for monitoring functionality.
-}

module Eclogues.Monitoring.ClusterSpec (spec) where

import qualified Eclogues.Job as Job
import Eclogues.Job (Satisfiability (..), UnsatisfiableReason (..))
import Eclogues.State.Types (AppState)
import Eclogues.Monitoring.Cluster (Cluster, updateSatisfiabilities)
import TestUtils (
      EitherError, Scheduler
    , nodeResources, halfResources, fullResources, overResources
    , isolatedJob, dependentJob, scheduler', scheduler, createJob'
    , shouldHave, satisfiability, forceName)

import Test.Hspec

{-# ANN module ("HLint: ignore Use ." :: String) #-}

job :: Job.Name
job = forceName "job"

dep :: Job.Name
dep = forceName "dep"

monitored :: Scheduler -> Cluster -> EitherError AppState
monitored sched cluster = do
    state <- scheduler' sched
    let monitor = updateSatisfiabilities (Just cluster) state
    scheduler state monitor

testCluster :: Cluster
testCluster = [nodeResources fullResources]

spec :: Spec
spec = do
    describe "updateSatisfiabilities" $
        it "should tag unsatisfiable job involving an unsatisfiable dependency as unsatisfiable" $
            let createdJobs = do
                    createJob' $ isolatedJob  dep       overResources
                    createJob' $ dependentJob job [dep] overResources
            in do
                monitored createdJobs testCluster `shouldHave` satisfiability dep (Unsatisfiable InsufficientResources)
                monitored createdJobs testCluster `shouldHave` satisfiability job (Unsatisfiable InsufficientResources)

    context "when a dependency requires unsatisfiable resources" $
        it "should mark the dependency unsatisfiable due to resources and the dependent unsatisfiable due to the dependency" $
            let createdJobs = do
                    createJob' $ isolatedJob  dep       overResources
                    createJob' $ dependentJob job [dep] halfResources
            in do
                monitored createdJobs testCluster `shouldHave` satisfiability dep (Unsatisfiable InsufficientResources)
                monitored createdJobs testCluster `shouldHave` satisfiability job (Unsatisfiable $ DependenciesUnsatisfiable [dep])

    context "when a dependent requires unsatisfiable resources" $
        it "should mark the dependency satisfiable and the dependent unsatisfiable due to resources" $
            let createdJobs = do
                    createJob' $ isolatedJob  dep       halfResources
                    createJob' $ dependentJob job [dep] overResources
            in do
                monitored createdJobs testCluster `shouldHave` satisfiability dep Satisfiable
                monitored createdJobs testCluster `shouldHave` satisfiability job (Unsatisfiable InsufficientResources)

    context "when a dependency and dependent both require unsatisfiable resources" $
        it "should mark both jobs unsatisfiable due to resources" $
            let createdJobs = do
                    createJob' $ isolatedJob  dep       overResources
                    createJob' $ dependentJob job [dep] overResources
            in do
                monitored createdJobs testCluster `shouldHave` satisfiability dep (Unsatisfiable InsufficientResources)
                monitored createdJobs testCluster `shouldHave` satisfiability job (Unsatisfiable InsufficientResources)

    context "when one dependency is satisfiable and another requires unsatisfiable resources" $
        it "should mark the dependent job as unsatisfiable due to the unsatisfiable dependency" $
            let createdJobs = do
                    createJob' $ isolatedJob  dep             overResources
                    createJob' $ isolatedJob  job             halfResources
                    createJob' $ dependentJob job2 [dep, job] halfResources
                job2 = forceName "job2"
            in do
                monitored createdJobs testCluster `shouldHave` satisfiability dep  (Unsatisfiable InsufficientResources)
                monitored createdJobs testCluster `shouldHave` satisfiability job  Satisfiable
                monitored createdJobs testCluster `shouldHave` satisfiability job2 (Unsatisfiable $ DependenciesUnsatisfiable [dep])
