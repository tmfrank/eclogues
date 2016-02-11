module Eclogues.MockSpec (spec) where

import Eclogues.API (API)
import qualified Eclogues.Job as Job
import qualified Eclogues.Mock as Mock

import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import Data.Word (Word16)
import Servant.API ((:<|>) (..))
import Servant.Client (BaseUrl (..), Scheme (Http), ServantError, client)
import TestUtils (isolatedJob', forceName)

import Test.Hspec

{-# ANN module ("HLint: ignore Use ." :: String) #-}

apiHost :: String
apiHost = "127.0.0.1"

apiPort :: Word16
apiPort = 8000

type Call a = EitherT ServantError IO a

getJobs :: Call [Job.Status]
createJob :: Job.Spec -> Call ()
jobStage :: Job.Name -> Call Job.Stage
(     getJobs
 :<|> _  -- jobStatus
 :<|> jobStage
 :<|> _  -- setJobStage
 :<|> _  -- deleteJob
 :<|> _  -- scheduler redirect
 :<|> _  -- output redirect
 :<|> createJob
 :<|> _  -- getHealth
 ) = client (Proxy :: Proxy API) (BaseUrl Http apiHost $ fromIntegral apiPort)

shouldGive :: (Eq a, Show a, Show e) => EitherT e IO a -> a -> Expectation
shouldGive ma b = runEitherT ma >>= \case
    Left e  -> expectationFailure $ show e
    Right a -> a `shouldBe` b

runningAPI :: (Mock.MockHelper -> IO a) -> IO a
runningAPI = Mock.runningMock apiHost apiPort

spec :: Spec
spec = describe "the mock API" $ do
    it "returns an empty list of jobs" $ runningAPI $ \_ ->
        getJobs `shouldGive` []
    it "should successfully create a job" $ runningAPI $ \_ -> do
        createJob (isolatedJob' $ forceName $ T.pack "test_jerb") `shouldGive` ()
        jobStage (forceName $ T.pack "test_jerb") `shouldGive` Job.Queued Job.LocalQueue
    it "it should create a job and then the job will complete after 0 ticks" $ runningAPI $ \h -> do
        createJob (isolatedJob' $ forceName $ T.pack "will_succeed_0_jerb") `shouldGive` ()
        Mock.waitForUpdate h
        jobStage (forceName $ T.pack "will_succeed_0_jerb") `shouldGive` Job.Finished
    it "it should create a job and then the job will complete after 3 ticks" $ runningAPI $ \i -> do
        createJob (isolatedJob' $ forceName $ T.pack "will_succeed_3_jerb") `shouldGive` ()
        Mock.waitForUpdate i
        Mock.waitForUpdate i
        Mock.waitForUpdate i
