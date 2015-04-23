{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AuroraConfig (TaskConfig, auroraJobConfig, lockKey) where

import Api_Types

import TaskSpec (TaskSpec (..), Resources (..))
import Units

import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.Encode (encodeToTextBuilder)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import qualified Data.HashSet as HashSet
import Data.Int (Int32, Int64)
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder (toLazyText)

encodeToText :: (ToJSON a) => a -> L.Text
encodeToText = toLazyText . encodeToTextBuilder . toJSON

data ATaskExecConf = ATaskExecConf   { tec_environment           :: L.Text
                                     , tec_task                  :: ATask
                                     , tec_name                  :: L.Text
                                     , tec_service               :: Bool
                                     , tec_max_task_failures     :: Int32
                                     , tec_cron_collision_policy :: CronCollisionPolicy
                                     , tec_priority              :: Int32
                                     , tec_cluster               :: L.Text
                                     , tec_health_check_config   :: AHealthCheckConfig
                                     , tec_role                  :: L.Text
                                     , tec_enable_hooks          :: Bool
                                     , tec_production            :: Bool }

data AResources = AResources { disk :: Int64
                             , ram  :: Int64
                             , cpu  :: Double }

data ATask = ATask   { task_processes          :: [AProcess]
                     , task_name               :: L.Text
                     , task_finalization_wait  :: Integer
                     , task_max_failures       :: Int32
                     , task_max_concurrency    :: Integer
                     , task_resources          :: AResources
                     , task_constraints        :: [ATaskConstraint] }

data ATaskConstraint = ATaskConstraint { order :: [L.Text] }

data AProcess = AProcess   { daemon       :: Bool 
                           , name         :: L.Text
                           , ephemeral    :: Bool
                           , max_failures :: Int32
                           , min_duration :: Integer
                           , cmdline      :: L.Text
                           , final        :: Bool }

data AHealthCheckConfig = AHealthCheckConfig   { initial_interval_secs    :: Double
                                               , interval_secs            :: Double
                                               , timeout_secs             :: Double
                                               , max_consecutive_failures :: Int32 }

$(deriveJSON defaultOptions ''CronCollisionPolicy)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''ATaskExecConf)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 5} ''ATask)
$(deriveJSON defaultOptions ''ATaskConstraint)
$(deriveJSON defaultOptions ''AProcess)
$(deriveJSON defaultOptions ''AResources)
$(deriveJSON defaultOptions ''AHealthCheckConfig)

defaultRole :: L.Text
defaultRole = "vagrant"

defaultOwner :: Identity
defaultOwner = Identity defaultRole defaultRole

defaultEnvironment :: L.Text
defaultEnvironment = "devel"

defaultJobKey :: L.Text -> JobKey
defaultJobKey name = JobKey defaultRole defaultEnvironment name

defaultConstraints :: HashSet.HashSet Constraint
defaultConstraints = HashSet.fromList [ Constraint { constraint_name = "host"
                                                   , constraint_constraint = taskc } ] where
    taskc = Api_Types.TaskConstraint { taskConstraint_value = default_ValueConstraint
                                     , taskConstraint_limit = limc }
    limc =  LimitConstraint { limitConstraint_limit = 1 }

defaultPriority :: Int32
defaultPriority = 0

defaultMaxTaskFailures :: Int32
defaultMaxTaskFailures = 1

defaultCluster :: L.Text
defaultCluster = "mesos_cluster"

defaultIsService :: Bool
defaultIsService = False

defaultProduction :: Bool
defaultProduction = False

defaultFinalisationWait :: Integer
defaultFinalisationWait = 30

defaultMaxConcurrency :: Integer
defaultMaxConcurrency = 0

defaultMinDuration :: Integer
defaultMinDuration = 5

defaultHealthCheckConfig :: AHealthCheckConfig
defaultHealthCheckConfig = AHealthCheckConfig  { initial_interval_secs    = 15
                                               , interval_secs            = 10
                                               , timeout_secs             = 1
                                               , max_consecutive_failures = 0 }

defaultCronCollisionPolicy :: CronCollisionPolicy
defaultCronCollisionPolicy = KILL_EXISTING

aResources :: Resources -> AResources
aResources (Resources disk ram cpu) = AResources (ceiling $ disk `asVal` byte) (ceiling $ ram `asVal` byte) (cpu `asVal` core)

auroraJobConfig :: TaskSpec -> JobConfiguration
auroraJobConfig (TaskSpec name cmd resources@(Resources disk ram cpu)) = job where
    jobKey = defaultJobKey name
    job = JobConfiguration  { jobConfiguration_key                 = jobKey
                            , jobConfiguration_owner               = defaultOwner
                            , jobConfiguration_cronSchedule        = Nothing
                            , jobConfiguration_cronCollisionPolicy = defaultCronCollisionPolicy
                            , jobConfiguration_taskConfig          = task
                            , jobConfiguration_instanceCount       = 1 }
    task = default_TaskConfig   { taskConfig_job             = jobKey
                                , taskConfig_owner           = defaultOwner
                                , taskConfig_environment     = defaultEnvironment
                                , taskConfig_jobName         = name
                                , taskConfig_isService       = defaultIsService
                                , taskConfig_numCpus         = cpu `asVal` core
                                , taskConfig_ramMb           = ceiling $ ram `asVal` mebi byte
                                , taskConfig_diskMb          = ceiling $ disk `asVal` mega byte
                                , taskConfig_priority        = defaultPriority
                                , taskConfig_maxTaskFailures = defaultMaxTaskFailures
                                , taskConfig_constraints     = HashSet.empty -- defaultConstraints
                                , taskConfig_requestedPorts  = HashSet.empty
                                , taskConfig_executorConfig  = Just execConf
                                , taskConfig_container       = Nothing }
    execConf = ExecutorConfig   { executorConfig_name = "AuroraExecutor"
                                , executorConfig_data = encodeToText aTaskExecConf }
    aTaskExecConf = ATaskExecConf   { tec_environment           = defaultEnvironment
                                    , tec_task                  = aTask
                                    , tec_name                  = name
                                    , tec_service               = defaultIsService
                                    , tec_max_task_failures     = defaultMaxTaskFailures
                                    , tec_cron_collision_policy = defaultCronCollisionPolicy
                                    , tec_priority              = defaultPriority
                                    , tec_cluster               = defaultCluster
                                    , tec_health_check_config   = defaultHealthCheckConfig
                                    , tec_role                  = defaultRole
                                    , tec_enable_hooks          = False
                                    , tec_production            = defaultProduction }
    aTask = ATask { task_processes          = [aProcess]
                  , task_name               = name
                  , task_finalization_wait  = defaultFinalisationWait
                  , task_max_failures       = defaultMaxTaskFailures
                  , task_max_concurrency    = defaultMaxConcurrency
                  , task_resources          = aResources resources
                  , task_constraints        = [orderConstraint] }
    orderConstraint = ATaskConstraint { order = [name] }
    aProcess = AProcess { daemon       = False
                        , name         = name
                        , ephemeral    = False
                        , max_failures = defaultMaxTaskFailures
                        , min_duration = defaultMinDuration
                        , cmdline      = cmd
                        , final        = False }

lockKey :: L.Text -> LockKey
lockKey = LockKey . defaultJobKey
