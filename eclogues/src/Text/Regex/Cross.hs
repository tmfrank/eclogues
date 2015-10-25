{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Regex.Cross (Regex, (=~), re) where

#ifdef GhcjsBase
{-# LANGUAGE JavascriptFFI #-}

import Data.JSString (JSString, pack)
import qualified Data.JSString.RegExp as R
import qualified Data.Text as T
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Lib (ExpQ)

type Regex = R.RegExp

foreign import javascript unsafe
  "new RegExp($1)" js_createRE :: JSString -> R.RegExp

quoteExpRegex :: String -> ExpQ
quoteExpRegex txt = [| js_createRE (pack txt) |]

re :: QuasiQuoter
re = QuasiQuoter { quoteExp  = quoteExpRegex
                 , quotePat  = undefined
                 , quoteType = undefined
                 , quoteDec  = undefined }

(=~) :: T.Text -> Regex -> Bool
(=~) = R.test . pack . T.unpack

#else

import Text.Regex.PCRE.Heavy (Regex, (=~), re)

{-# ANN module "HLint: ignore Unused LANGUAGE pragma" #-}

#endif
