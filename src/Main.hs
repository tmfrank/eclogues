{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Api_Types

import AuroraAPI (Client, JobConfiguration, thriftClient, getJobs, createJob)
import AuroraConfig (taskSpec, auroraJobConfig)
import TaskSpec (TaskSpec (..))
import Units

import Control.Applicative ((<$>), pure)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT (..), bimapEitherT)
import Data.Aeson (encode, decode)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Either (lefts, rights)
import Data.HashSet (HashSet, toList)
import Data.Proxy (Proxy (Proxy))
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.URI (parseURI)
import Network.Wai.Handler.Warp (run)
import Servant.API ((:>), (:<|>) ((:<|>)), Get, ReqBody, Post)
import Servant.Server (Server, serve)

type VAPI =  "jobs"   :> Get [TaskSpec]
        :<|> "create" :> ReqBody TaskSpec :> Post ()

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
infixl 4 <<$>>

server :: Client -> Server VAPI
server client = getJobsH :<|> createJobH where
    getJobsH = bimapEitherT onError id $ do
        jobSet <- EitherT $ getJobs client
        let jobs  = toList jobSet
        let taskEs = taskSpec <$> jobs
        liftIO $ mapM_ putStrLn $ lefts taskEs
        pure $ rights taskEs
    createJobH = bimapEitherT onError id . EitherT . createJob client
    onError res = (500, show res)

main :: IO ()
main = do
    let (Just uri) = parseURI "http://192.168.100.3:8081/api"
    client <- thriftClient uri

    run 8000 $ serve (Proxy :: (Proxy VAPI)) (server client)
