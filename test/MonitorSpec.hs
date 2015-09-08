{-# LANGUAGE FlexibleContexts #-}
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

module MonitorSpec where

import Eclogues.Job as Job
import Eclogues.State.Types
import Eclogues.Monitoring.Cluster
import TestUtils

import qualified Test.Hspec as Hspec
import Test.Hspec (describe, context, it)
import Control.Lens (view, (^.))
import qualified Data.HashMap.Lazy as HashMap

job :: Job.Name
job = forceName "job"

dep :: Job.Name
dep = forceName "dep"

testUpdateSatisfiabilities :: Hspec.Spec
testUpdateSatisfiabilities = let
        satisfiability :: Name -> Satisfiability -> EitherError AppState -> EitherError Bool
        satisfiability jName jSatis aState = do
            state <- aState
            let status = HashMap.lookup jName $ state ^. jobs
            return $ maybe False ((== jSatis) . view satis) status

        testCluster :: Cluster
        testCluster = HashMap.insert "testMachine" fullResources HashMap.empty
    in do
        describe "updateSatisfy" $
            it "should tag satisfiable jobs involving a dependency as Satisfiable" $
                let createdJobs = do
                        createWithCluster testCluster $ isolatedJob  dep       halfResources
                        createWithCluster testCluster $ dependentJob job [dep] halfResources
                in do
                    scheduler' createdJobs `shouldHave` satisfiability dep Satisfiable
                    scheduler' createdJobs `shouldHave` satisfiability job Satisfiable

        context "when a dependency requires unsatisfiable resources" $
            it "should mark the dependency unsatisfiable due to resources and the dependent unsatisfiable due to the dependency" $
                let createdJobs = do
                        createWithCluster testCluster $ isolatedJob  dep       overResources
                        createWithCluster testCluster $ dependentJob job [dep] halfResources
                in do
                    scheduler' createdJobs `shouldHave` satisfiability dep (Unsatisfiable InsufficientResources)
                    scheduler' createdJobs `shouldHave` satisfiability job (Unsatisfiable $ DependenciesUnsatisfiable [dep])

        context "when a dependent requires unsatisfiable resources" $
            it "should mark the dependency satisfiable and the dependent unsatisfiable due to resources" $
                let createdJobs = do
                        createWithCluster testCluster $ isolatedJob  dep       halfResources
                        createWithCluster testCluster $ dependentJob job [dep] overResources
                in do
                    scheduler' createdJobs `shouldHave` satisfiability dep Satisfiable
                    scheduler' createdJobs `shouldHave` satisfiability job (Unsatisfiable InsufficientResources)

        context "when a dependency and dependent both require unsatisfiable resources" $
            it "should mark both jobs unsatisfiable due to resources" $
                let createdJobs = do
                        createWithCluster testCluster $ isolatedJob  dep       overResources
                        createWithCluster testCluster $ dependentJob job [dep] overResources
                in do
                    scheduler' createdJobs `shouldHave` satisfiability dep (Unsatisfiable InsufficientResources)
                    scheduler' createdJobs `shouldHave` satisfiability job (Unsatisfiable InsufficientResources)

        context "when one dependency is satisfiable and another requires unsatisfiable resources" $
            it "should mark the dependent job as unsatisfiable due to the unsatisfiable dependency" $
                let createdJobs = do
                        createWithCluster testCluster $ isolatedJob  dep             overResources
                        createWithCluster testCluster $ isolatedJob  job             halfResources
                        createWithCluster testCluster $ dependentJob job2 [dep, job] halfResources
                    job2 = forceName "job2"
                in do
                    scheduler' createdJobs `shouldHave` satisfiability dep  (Unsatisfiable InsufficientResources)
                    scheduler' createdJobs `shouldHave` satisfiability job  Satisfiable
                    scheduler' createdJobs `shouldHave` satisfiability job2 (Unsatisfiable $ DependenciesUnsatisfiable [dep])

testMonitor :: Hspec.Spec
testMonitor = testUpdateSatisfiabilities
