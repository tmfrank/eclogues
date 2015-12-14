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

module Eclogues.Job.Aeson where

import Data.Char (toLower)

specJName :: String -> String
specJName x = toLower <$> drop 2 x

statusJName :: String -> String
statusJName "__satis" = "satisfiability"
statusJName x         = toLower <$> drop 2 x
