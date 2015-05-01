{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import TaskAPI ( AppState, JobStatus, JobError (..), newAppState
               , createJob, killJob, getJob, getJobs, updateJobs )
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
import Servant.API ((:>), (:<|>) ((:<|>)), Get, Post, Delete, ReqBody, Capture)
import Servant.Common.Text (FromText (..))
import Servant.Server (Server, serve)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

instance FromText L.Text where
    fromText = fmap L.fromStrict . fromText

type VAPI =  "jobs"   :> Get [JobStatus]
        :<|> "job"    :> Capture "id" Name :> Get JobStatus
        :<|> "job"    :> Capture "id" Name :> Delete
        :<|> "create" :> ReqBody TaskSpec  :> Post ()

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
infixl 4 <<$>>

server :: AppState -> Server VAPI
server appState = getJobsH :<|> getJobH :<|> killJobH :<|> createJobH where
    getJobsH = lift $ getJobs appState
    getJobH jid = EitherT $ maybe (Left (404, "")) Right <$> getJob appState jid
    killJobH = toEitherT . withExceptT onError . killJob appState
    createJobH = toEitherT . withExceptT onError . createJob appState
    onError e = case e of
        UnknownResponse res -> (500, show res)
        NoSuchJob           -> (404, "")
    toEitherT = EitherT . runExceptT

main :: IO ()
main = do
    (hostArg:_) <- getArgs
    let (Just uri) = parseURI $ "http://" ++ hostArg ++ "/api"
    appState <- newAppState uri "./jobs"

    hPutStrLn stderr "Starting server on port 8000"

    let web = run 8000 $ serve (Proxy :: (Proxy VAPI)) (server appState)
    let updater = forever $ updateJobs appState >> threadDelay (floor $ second 1 `asVal` micro second)
    race_ web updater
