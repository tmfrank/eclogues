{-# LANGUAGE OverloadedStrings #-}

module Eclogues.Scheduling.CommandSpec (spec) where

import Api_Types (
      AssignedTask (..), ScheduledTask (..), TaskEvent (..)
    , jobConfiguration_taskConfig)
import Api_Types2 (ScheduleStatus (..))
import Eclogues.Job (
      Name, Stage (..), QueueStage (SchedulerQueue)
    , FailureReason (UserKilled), RunErrorReason (SubexecutorFailure)
    , nameText)
import Eclogues.Scheduling.AuroraConfig (auroraJobConfig)
import Eclogues.Scheduling.Command (lookupNewStages)
import TestUtils (isolatedJob', forceName)

import qualified Data.HashMap.Strict as HM
import Data.Text.Lazy (fromStrict)
import Data.Vector (fromList)

import Test.Hspec

auroraTask :: Name -> ScheduleStatus -> [TaskEvent] -> ScheduledTask
auroraTask name stat evs = st
  where
    role = "vagrant"
    jobSpec = isolatedJob' name
    st = ScheduledTask { scheduledTask_assignedTask = at
                       , scheduledTask_status       = stat
                       , scheduledTask_failureCount = 1
                       , scheduledTask_taskEvents   = fromList evs
                       , scheduledTask_ancestorId   = "" }
    at = AssignedTask  { assignedTask_taskId        = fromStrict $ nameText name
                       , assignedTask_slaveId       = ""
                       , assignedTask_slaveHost     = ""
                       , assignedTask_task          = tc
                       , assignedTask_assignedPorts = HM.empty
                       , assignedTask_instanceId    = 0 }
    tc = jobConfiguration_taskConfig $ auroraJobConfig role jobSpec

taskEvent :: ScheduleStatus -> TaskEvent
taskEvent st = TaskEvent { taskEvent_timestamp = 0
                         , taskEvent_status    = st
                         , taskEvent_message   = Nothing
                         , taskEvent_scheduler = Nothing }

spec :: Spec
spec = describe "lookupNewStages" $ do
    let name = forceName "test"
        uuidMap = [(name, name)]
        shouldResolveTo ts r = lookupNewStages uuidMap ts `shouldBe` [(name, r)]

        lost        = auroraTask name LOST     []
        running     = auroraTask name RUNNING  []
        finished    = auroraTask name FINISHED []
        killed      = auroraTask name KILLED   [taskEvent KILLED]
        failed      = auroraTask name FAILED   [taskEvent FAILED]
        lostKilling = auroraTask name LOST     [taskEvent KILLING]

    it "converts Aurora LOST to Queued SchedulerQueue" $
        [lost]              `shouldResolveTo` Queued SchedulerQueue

    it "picks Aurora RUNNING over LOST" $
        [lost, running]     `shouldResolveTo` Running

    it "picks Aurora FINISHED over LOST" $
        [lost, finished]    `shouldResolveTo` Finished

    it "picks Aurora KILLED over LOST" $
        [lost, killed]      `shouldResolveTo` Failed UserKilled

    it "picks Aurora FAILED over LOST" $
        [lost, failed]      `shouldResolveTo` RunError SubexecutorFailure

    it "picks Aurora LOST after KILLING over LOST" $
        [lost, lostKilling] `shouldResolveTo` Failed UserKilled
