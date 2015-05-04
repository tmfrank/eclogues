{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import TaskAPI ( AppState, JobStatus (jobState), JobError (..), newAppState
               , createJob, killJob, deleteJob, getJob, getJobs, updateJobs )
import TaskSpec (TaskSpec (..), Name, JobState (..), FailureReason (..))
import Units

import Control.Applicative ((<$>), (<*))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT (..), left)
import Control.Monad.Trans.Except (runExceptT, withExceptT)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text.Lazy as L
import Network.URI (parseURI)
import Network.Wai.Handler.Warp (run)
import Servant.API ((:>), (:<|>) ((:<|>)), Get, Post, Put, Delete, ReqBody, Capture)
import Servant.Common.Text (FromText (..))
import Servant.Server (Server, serve)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

instance FromText L.Text where
    fromText = fmap L.fromStrict . fromText

type VAPI =  "jobs"   :> Get [JobStatus]
        :<|> "job"    :> Capture "id" Name :> Get JobStatus
        :<|> "job"    :> Capture "id" Name :> "state" :> Get JobState
        :<|> "job"    :> Capture "id" Name :> "state" :> ReqBody JobState :> Put ()
        :<|> "job"    :> Capture "id" Name :> Delete
        :<|> "create" :> ReqBody TaskSpec  :> Post ()

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
infixl 4 <<$>>

bool :: a -> a -> Bool -> a
bool a b p = if p then b else a

server :: AppState -> Server VAPI
server appState = getJobsH :<|> getJobH :<|> getJobStateH :<|> killJobH :<|> deleteJobH :<|> createJobH where
    getJobsH = lift $ getJobs appState
    getJobH jid = EitherT $ maybe (Left (404, "")) Right <$> getJob appState jid
    getJobStateH = fmap jobState . getJobH
    createJobH = toEitherT . withExceptT onError . createJob appState
    deleteJobH = toEitherT . withExceptT onError . deleteJob appState

    killJobH jid (Failed UserKilled) = toEitherT $ withExceptT onError $ killJob appState jid
    killJobH jid _                   = left (409, "Can only set state to Failed UserKilled") <* getJobH jid

    onError e = case e of
        UnexpectedResponse res -> (500, show res)
        NoSuchJob              -> (404, "")
        JobMustBeTerminated yn -> (409, "Job " ++ (bool "must not" "must" yn) ++ " be terminated")
        JobNameUsed            -> (409, "Job name already used")
    toEitherT = EitherT . runExceptT

main :: IO ()
main = do
    (jobsDir:hostArg:_) <- getArgs
    let (Just uri) = parseURI $ "http://" ++ hostArg ++ "/api"
    createDirectoryIfMissing False jobsDir
    appState <- newAppState uri jobsDir

    hPutStrLn stderr "Starting server on port 8000"

    let web = run 8000 $ serve (Proxy :: (Proxy VAPI)) (server appState)
    let updater = forever $ updateJobs appState >> threadDelay (floor $ second 1 `asVal` micro second)
    race_ web updater
