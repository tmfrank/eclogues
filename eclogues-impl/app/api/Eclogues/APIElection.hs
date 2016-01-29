{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

Functions for Eclogues API instance election.
-}

module Eclogues.APIElection (
    LeadershipError (..), ManagedZK, ZKURI, whileLeader, advertisedData
    ) where

import Eclogues.API (zkNode)

import Control.Monad.Except (ExceptT, throwError, catchError)
import Control.Monad.Trans.Control (liftBaseOp)
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Word (Word16)
import Database.Zookeeper.Election (LeadershipError (..), whenLeader)
import Database.Zookeeper.ManagedEvents (ManagedZK, ZKURI, withZookeeper)

-- | Contest Zookeeper election with the provided data, and perform some
-- action while elected. If leadership is lost, wait until re-elected and
-- perform the action again.
--
-- This function will never throw 'LeadershipLost'.
whileLeader :: ZKURI
            -> String    -- ^ API host
            -> Word16    -- ^ API port
            -> (ManagedZK -> IO a)
            -> ExceptT LeadershipError IO a
whileLeader zkUri host port act = liftBaseOp (withZookeeper zkUri) go
  where
    go zk = catchError (whenLeader zk zkNode zkData $ act zk) $ \case
        LeadershipLost -> go zk
        e              -> throwError e
    zkData = advertisedData host port

-- | Create encoded (host, port) to advertise via Zookeeper.
advertisedData :: String -> Word16 -> ByteString
advertisedData host port = toStrict $ encode (host, port)
