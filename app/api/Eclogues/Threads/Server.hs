{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

Web server thread.
-}

module Eclogues.Threads.Server (serve) where

import Prelude hiding ((.))

import Eclogues.API (VAPI, JobError (..), Health (Health))
import Eclogues.ApiDocs (VAPIWithDocs, apiDocsHtml)
import Eclogues.AppConfig (AppConfig)
import qualified Eclogues.AppConfig as Config
import Eclogues.Job (FailureReason (UserKilled))
import qualified Eclogues.Job as Job
import qualified Eclogues.Persist as Persist
import Eclogues.ServantInstances ()
import Eclogues.State (getJobs, createJob, killJob, deleteJob, getJob)
import qualified Eclogues.State.Monad as ES
import Eclogues.State.Types (AppState)
import Eclogues.Util (maybeDo)

import Control.Category ((.))
import Control.Concurrent.AdvSTM (atomically, onCommit)
import Control.Concurrent.AdvSTM.TVar (TVar, readTVar, writeTVar)
import Control.Concurrent.AdvSTM.TChan (writeTChan)
import Control.Lens ((^.))
import Control.Monad ((<=<))
import Control.Monad.State (State)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT (EitherT))
import Control.Monad.Trans.Except (ExceptT, runExceptT, withExceptT, mapExceptT, throwE)
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BSSC
import Data.Maybe (isJust, fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import Network.HTTP.Types (methodGet, methodPost, methodDelete, methodPut)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import Path (mkAbsFile)
import Servant.API ((:<|>) ((:<|>)))
import Servant.Server ( Server, ServerT, ServantErr (..), (:~>) (..)
                      , enter, err404, err409, err503, err303 )
import qualified Servant.Server as Server
import System.Random (randomIO)

-- | Start serving.
serve :: String        -- ^ Host interface to bind to
      -> Int           -- ^ Port to listen on
      -> AppConfig     -- ^ App config
      -> TVar AppState -- ^ Mutable app state
      -> IO ()
serve host port conf = Warp.runSettings settings . myCors . Server.serve (Proxy :: (Proxy VAPIWithDocs)) . server conf
  where
    myCors = cors . const $ Just corsPolicy
    server c = otherwiseShowDocs . mainServer c
    settings = Warp.setHost (fromString host) $ Warp.setPort port Warp.defaultSettings

mainServer :: AppConfig -> TVar AppState -> Server VAPI
mainServer conf stateV = handleExcept server' where
    server' :: ServerT VAPI (ExceptT JobError IO)
    server' = getJobsH :<|> getJobH :<|> getJobStageH :<|> killJobH :<|>
              mesosJob :<|> outputH :<|> deleteJobH :<|> createJobH :<|> healthH

    healthH = lift $ Health . isJust <$> atomically (Config.auroraURI conf)
    getJobsH = lift $ getJobs <$> atomically (readTVar stateV)
    getJobH jid = getJob jid =<< lift (atomically $ readTVar stateV)
    getJobStageH = fmap (^. Job.stage) . getJobH
    createJobH spec = lift randomIO >>= runScheduler' . flip createJob spec
    deleteJobH = runScheduler' . deleteJob
    mesosJob = toMesos <=< getJobH where
        toMesos :: Job.Status -> ExceptT JobError IO ()
        toMesos js = (lift . atomically $ Config.auroraURI conf) >>= \case
            Nothing  -> throwE SchedulerInaccessible
            Just uri -> throwE . SchedulerRedirect . Config.schedJobURI conf uri $ js ^. Job.uuid
    outputH name pathM =
        getJobH name *>
        throwE (SchedulerRedirect $ Config.outputURI conf name path)
      where
        path = fromMaybe $(mkAbsFile "/stdout") pathM

    killJobH jid (Job.Failed UserKilled) = runScheduler' $ killJob jid
    killJobH jid _                       = do
        _ <- getJobH jid
        throwE $ InvalidStageTransition "Can only set stage to Failed UserKilled"
    runScheduler' = runScheduler conf stateV

handleExcept :: ServerT VAPI (ExceptT JobError IO) -> Server VAPI
handleExcept = enter $ fromExceptT . Nat (withExceptT onError)

onError :: JobError -> ServantErr
onError e = case e of
    NoSuchJob             -> err404 { errBody = encode NoSuchJob }
    SchedulerInaccessible -> err503 { errBody = encode SchedulerInaccessible }
    SchedulerRedirect uri -> err303 { errHeaders = [("Location", BSSC.pack (show uri))] }
    other                 -> err409 { errBody = encode other }

type Scheduler = ExceptT JobError (State ES.TransitionaryState) ()

runScheduler :: AppConfig -> TVar AppState -> Scheduler -> ExceptT JobError IO ()
runScheduler conf stateV f = mapExceptT atomically $ do
    state <- lift $ readTVar stateV
    case ES.runState state $ runExceptT f of
        (Left  e, _ ) -> throwE e
        (Right _, ts) -> lift $ do
            mapM_ (writeTChan $ Config.schedChan conf) $ ts ^. ES.scheduleCommands
            writeTVar stateV $ ts ^. ES.appState
            maybeDo $ onCommit . Persist.atomically (Config.pctx conf) <$> ts ^. ES.persist

otherwiseShowDocs :: Server VAPI -> Server VAPIWithDocs
otherwiseShowDocs = (:<|> serveDocs) where
    serveDocs = lift $ pure apiDocsHtml

fromExceptT :: ExceptT e m :~> EitherT e m
fromExceptT = Nat $ \x -> EitherT $ runExceptT x

corsPolicy :: CorsResourcePolicy
corsPolicy = CorsResourcePolicy { corsOrigins = Nothing
                                , corsMethods = [methodGet, methodPost, methodDelete, methodPut]
                                , corsRequestHeaders = ["content-type"]
                                , corsExposedHeaders = Nothing
                                , corsMaxAge = Nothing
                                , corsVaryOrigin = False
                                , corsRequireOrigin = False
                                , corsIgnoreFailures = False }
