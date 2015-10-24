{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

module Eclogues.Paths where

import Path (Path, Rel, File, mkRelFile)

runResult :: Path Rel File
runResult = $(mkRelFile "runresult")

specFile :: Path Rel File
specFile = $(mkRelFile "spec.json")
