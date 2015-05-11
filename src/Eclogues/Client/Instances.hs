{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Eclogues.Client.Instances where

import Control.Monad (void)
import Control.Monad.Trans.Either (EitherT)
import Data.Proxy (Proxy (Proxy))
import qualified Network.HTTP.Types as H
import Servant.API (Put, Delete, Post)
import Servant.Client (HasClient (..), ServantError)
import Servant.Common.Req (performRequestNoBody)

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
  HasClient (Put '[] ()) where
  type Client (Put '[] ()) = EitherT ServantError IO ()
  clientWithRoute Proxy req baseurl =
    void $ performRequestNoBody H.methodPut req [204] baseurl

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
  HasClient (Delete '[] ()) where
  type Client (Delete '[] ()) = EitherT ServantError IO ()
  clientWithRoute Proxy req baseurl =
    void $ performRequestNoBody H.methodDelete req [204] baseurl

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
  HasClient (Post '[] ()) where
  type Client (Post '[] ()) = EitherT ServantError IO ()
  clientWithRoute Proxy req baseurl =
    void $ performRequestNoBody H.methodPost req [204] baseurl