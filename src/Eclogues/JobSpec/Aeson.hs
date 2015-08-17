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
specJName "__jobResources" = "resources"
specJName x                = toLower <$> drop 1 x

statusJName :: String -> String
statusJName "__jobSpec" = "spec"
statusJName "_jobState" = "state"
statusJName x           = toLower <$> drop 1 x
