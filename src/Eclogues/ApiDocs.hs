{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Eclogues.ApiDocs (apiDocs, apiDocsMd, apiDocsHtml) where

import Eclogues.API (VAPI, Health (Health))
import Eclogues.TaskSpec (TaskSpec (..), Resources (..), JobState (..), JobStatus (..), FailureReason (..), Name, taskCommand)
import Units

import Control.Lens ((.~), (&))
import qualified Data.ByteString.Lazy as L
import Data.Proxy (Proxy (..))
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.UUID (nil)
import Text.Pandoc (writeHtmlString, def)
import Servant.API (Capture)
import Servant.Docs (API, ToCapture (..), ToSample (..), DocCapture (..), docs, markdown)
import Servant.Docs.Pandoc (pandoc)

instance ToCapture (Capture "name" Name) where
    toCapture _ = DocCapture "name" "job name"

instance ToSample JobState JobState where
    toSamples _ = [("A running task", Running)
                  ,("A task killed by a user", Failed UserKilled)]

res :: Resources
res = (Resources (mega byte 10) (mebi byte 10) (core 0.1) (second 5))

spec :: TaskSpec
spec = TaskSpec "hello" "echo hello world > hello.txt" res ["hello.txt"] False []

depSpec :: TaskSpec
depSpec = TaskSpec "cat-hello" "cat yb-dependencies/hello/hello.txt" res [] True ["hello"]

instance ToSample TaskSpec TaskSpec where
    toSamples _ = [("A job with output", spec)
                  ,("A job depending on previous output", depSpec)]

failedSpec :: JobStatus
failedSpec = JobStatus deadSpec (Failed $ NonZeroExitCode 1) nil where
    deadSpec = TaskSpec "i-fail" "exit 1" res [] False []

instance ToSample JobStatus JobStatus where
    toSample _ = Just $ failedSpec

instance ToSample [JobStatus] [JobStatus] where
    toSample _ = Just [ JobStatus (spec & taskCommand .~ "cat /dev/zero > hello.txt") (Failed TimeExceeded) nil
                      , JobStatus depSpec (Failed $ DependencyFailed "hello") nil]

instance ToSample () () where
    toSample _ = Nothing

instance ToSample Health Health where toSample _ = Just $ Health True

apiDocs :: API
apiDocs = docs (Proxy :: Proxy VAPI)

apiDocsMd :: L.ByteString
apiDocsMd = encodeUtf8 . pack $ markdown apiDocs

apiDocsHtml :: L.ByteString
apiDocsHtml = encodeUtf8 . pack . writeHtmlString def $ pandoc apiDocs
