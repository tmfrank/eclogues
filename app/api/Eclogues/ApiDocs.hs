{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

"Eclogues.API" documentation via "Servant.Docs.Pandoc" and associates.
-}

module Eclogues.ApiDocs (VAPIWithDocs, apiDocs, apiDocsMd, apiDocsHtml) where

import Eclogues.API (VAPI, Health (Health), AbsFile)
import Eclogues.Job (Stage (Failed, Running), FailureReason (..), Satisfiability (Satisfiable))
import qualified Eclogues.Job as Job
import Units

import Control.Lens ((.~), (&))
import qualified Data.ByteString.Lazy as L
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.UUID (nil)
import Network.HTTP.Media ((//), (/:))
import Path (mkAbsFile)
import Text.Pandoc (writeHtmlString, def)
import Servant.API (Capture, QueryParam, Get, Accept (..), MimeRender (..), (:>), (:<|>))
import Servant.Docs ( API, ToCapture (..), ToSample (..), ToParam (..),
                      DocCapture (..), DocQueryParam (..), ParamKind (Normal),
                      docs, markdown )
import Servant.Docs.Pandoc (pandoc)

instance ToCapture (Capture "name" Job.Name) where
    toCapture _ = DocCapture "name" "job name"

instance ToParam (QueryParam "path" AbsFile) where
    toParam _ = DocQueryParam "path" ["stdout", "JOB_SPECIFIC_PATH"] "output file path" Normal

instance ToSample Job.Stage Job.Stage where
    toSamples _ = [("A running task", Running)
                  ,("A task killed by a user", Failed UserKilled)]

res :: Job.Resources
res = Job.Resources (mega byte 10) (mebi byte 10) (core 0.1) (second 5)

helloName :: Job.Name
helloName = fromJust $ Job.mkName "hello"

spec :: Job.Spec
spec = Job.Spec helloName "echo hello world > hello.txt" res [Job.OutputPath $(mkAbsFile "/hello.txt")] False []

depSpec :: Job.Spec
depSpec = Job.Spec n "cat virgil-dependencies/hello/hello.txt" res [] True [helloName]
  where
    n = fromJust $ Job.mkName "cat-hello"

instance ToSample Job.Spec Job.Spec where
    toSamples _ = [("A job with output", spec)
                  ,("A job depending on previous output", depSpec)]

failedSpec :: Job.Status
failedSpec = Job.Status deadSpec (Failed $ NonZeroExitCode 1) Satisfiable nil where
    deadSpec = Job.Spec n "exit 1" res [] False []
    n = fromJust $ Job.mkName "i-fail"

instance ToSample Job.Status Job.Status where
    toSample _ = Just failedSpec

instance ToSample [Job.Status] [Job.Status] where
    toSample _ = Just [ Job.Status (spec & Job.command .~ "cat /dev/zero > hello.txt") (Failed TimeExceeded) Satisfiable nil
                      , Job.Status depSpec (Failed $ DependencyFailed helloName) Satisfiable nil]

instance ToSample () () where
    toSample _ = Nothing

instance ToSample Health Health where toSample _ = Just $ Health True

apiDocs :: API
apiDocs = docs (Proxy :: Proxy VAPI)

apiDocsMd :: L.ByteString
apiDocsMd = encodeUtf8 . pack $ markdown apiDocs

apiDocsHtml :: L.ByteString
apiDocsHtml = encodeUtf8 . pack . writeHtmlString def $ pandoc apiDocs

type VAPIWithDocs = VAPI :<|> "api" :> Get '[HTML] L.ByteString

data HTML

instance Accept HTML where contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML L.ByteString where mimeRender _ html = html
