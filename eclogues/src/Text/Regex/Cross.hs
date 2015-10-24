{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Regex.Cross (Regex, (=~), re) where

#ifdef GhcjsBase

import Data.JSString (pack)
import qualified Data.JSString.RegExp as R
import qualified Data.Text as T
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Lib (ExpQ)

type Regex = R.RegExp

quoteExpRegex :: String -> ExpQ
quoteExpRegex txt = [| R.create (R.REFlags False False) (pack txt) |]

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
