{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import TaskAPI ( AppState, JobStatus, newAppState
               , createJob, getJob, getJobs, updateJobs )
import TaskSpec (TaskSpec (..), Name)
import Units

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT (..))
import Control.Monad.Trans.Except (runExceptT, withExceptT)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text.Lazy as L
import Network.URI (parseURI)
import Network.Wai.Handler.Warp (run)
import Servant.API ((:>), (:<|>) ((:<|>)), Get, ReqBody, Post, Capture)
import Servant.Common.Text (FromText (..))
import Servant.Server (Server, serve)
import System.IO (hPutStrLn, stderr)

instance FromText L.Text where
    fromText = fmap L.fromStrict . fromText

type VAPI =  "jobs"   :> Get [JobStatus]
        :<|> "job"    :> Capture "id" Name :> Get JobStatus
        :<|> "create" :> ReqBody TaskSpec  :> Post ()

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
infixl 4 <<$>>

server :: AppState -> Server VAPI
server appState = getJobsH :<|> getJobH :<|> createJobH where
    getJobsH = lift $ getJobs appState
    getJobH jid = EitherT $ maybe (Left (404, "")) Right <$> getJob appState jid
    createJobH = toEitherT . withExceptT onError . createJob appState
    onError e = (500, show e)
    toEitherT = EitherT . runExceptT

main :: IO ()
main = do
    let (Just uri) = parseURI "http://192.168.100.3:8081/api"
    appState <- newAppState uri "./jobs"

    hPutStrLn stderr "Starting server on port 8000"

    let web = run 8000 $ serve (Proxy :: (Proxy VAPI)) (server appState)
    let updater = forever $ updateJobs appState >> threadDelay (floor $ second 1 `asVal` micro second)
    race_ web updater
