module MockSpec (testMock) where

import Eclogues.API (VAPI)
import qualified Eclogues.Job as Job
import qualified Mock

import Control.Concurrent.MVar (putMVar, takeMVar, newEmptyMVar)
import Control.Concurrent.Async (withAsync)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.Proxy (Proxy (Proxy))
import Data.Word (Word16)
import Servant ((:<|>) (..))
import Servant.Client (BaseUrl (..), Scheme (Http), ServantError, client)

import Test.Hspec

apiHost :: String
apiHost = "127.0.0.1"

apiPort :: Word16
apiPort = 8000

type Call a = EitherT ServantError IO a

getJobs :: Call [Job.Status]
(     getJobs
 :<|> _  -- jobStatus
 :<|> _  -- jobStage
 :<|> _  -- setJobStage
 :<|> _  -- deleteJob
 :<|> _  -- scheduler redirect
 :<|> _  -- output redirect
 :<|> _  -- createJob
 :<|> _  -- getHealth
 ) = client (Proxy :: Proxy VAPI) (BaseUrl Http apiHost $ fromIntegral apiPort)

runningAPI :: Expectation -> Expectation
runningAPI a = do
    go <- newEmptyMVar
    withAsync (Mock.run (putMVar go ()) apiHost apiPort) . const $ do
        takeMVar go
        a

shouldGive :: (Eq a, Show a, Show e) => EitherT e IO a -> a -> Expectation
shouldGive ma b = runEitherT ma >>= \case
    Left e  -> expectationFailure $ show e
    Right a -> a `shouldBe` b

testMock :: Spec
testMock =
    describe "the mock API" $
        it "returns an empty list of jobs" $ runningAPI $
            getJobs `shouldGive` []
