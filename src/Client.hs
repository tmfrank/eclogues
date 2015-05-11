module Main where

import Eclogues.Util (orShowError)
import Eclogues.Client (EcloguesClient (..), ecloguesClient)

import Control.Monad ((<=<), when)
import Control.Monad.Trans.Except (runExceptT)
import Data.Maybe (isNothing)
import System.Environment (getArgs)

main :: IO ()
main = do
    (zkUri:_) <- getArgs
    clientM <- (orShowError =<<) . runExceptT $ ecloguesClient zkUri
    when (isNothing clientM) $ error "No Eclogues server advertised"
    let Just client = clientM
    ((print <=< orShowError) =<<) . runExceptT $ getJobs client
