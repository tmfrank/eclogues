{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

Template Haskell stage restricted definitions for JobSpec Aeson instances.
-}

module Eclogues.JobSpec.Aeson where

import Data.Char (toLower)

specJName :: String -> String
specJName "_job_resources" = "resources"
specJName x                = fmap toLower $ drop 1 x

statusJName :: String -> String
statusJName "_job_spec" = "spec"
statusJName "_jobState" = "state"
statusJName x           = fmap toLower $ drop 1 x
