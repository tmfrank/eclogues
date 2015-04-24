{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Api_Types

import AuroraAPI (Client, JobConfiguration, thriftClient, getJobs)
import TaskSpec (Resources (..), TaskSpec (..))
import Units

import Data.Aeson.TH (deriveJSON, defaultOptions)
import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT (..))
import Data.Either.Combinators (mapBoth)
import Data.HashSet (HashSet)
import Data.Proxy (Proxy (Proxy))
import Network.URI (parseURI)
import Network.Wai.Handler.Warp (run)
import Servant.API ((:>), Get)
import Servant.Server (Server, serve)

$(deriveJSON defaultOptions ''JobConfiguration)
$(deriveJSON defaultOptions ''Identity)
$(deriveJSON defaultOptions ''JobKey)
$(deriveJSON defaultOptions ''TaskConfig)
$(deriveJSON defaultOptions ''Constraint)
$(deriveJSON defaultOptions ''TaskConstraint)
$(deriveJSON defaultOptions ''LimitConstraint)
$(deriveJSON defaultOptions ''ValueConstraint)
$(deriveJSON defaultOptions ''Container)
$(deriveJSON defaultOptions ''DockerContainer)
$(deriveJSON defaultOptions ''MesosContainer)
$(deriveJSON defaultOptions ''ExecutorConfig)
$(deriveJSON defaultOptions ''Metadata)

type VAPI = "jobs" :> Get (HashSet JobConfiguration)

server :: Client -> Server VAPI
server client = getJobsH where
    getJobsH    = EitherT $ mapBoth onError id <$> getJobs client
    onError res = (500, show res)

main :: IO ()
main = do
    let (Just uri) = parseURI "http://192.168.100.3:8081/api"
    client <- thriftClient uri

    run 8000 $ serve (Proxy :: (Proxy VAPI)) (server client)
