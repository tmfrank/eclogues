{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (readFile)

import Eclogues.Util (orShowError)
import Eclogues.Client (EcloguesClient (..), ecloguesClient)
import Eclogues.Zookeeper (ZKURI)

import Control.Applicative ((<$>), (<*>), pure, optional)
import Control.Monad ((<=<), when)
import Control.Monad.Loops (firstM)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (eitherDecode)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString.Lazy (readFile)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Options.Applicative ( Parser, strOption, long, metavar, help
                           , execParser, info, helper, fullDesc, header )
import System.Directory (doesFileExist)
import System.Environment.XDG.BaseDir (getAllConfigFiles)

data Opts = Opts { zkUriM :: Maybe ZKURI }

data ClientConfig = ClientConfig { zookeeperHosts :: ZKURI }

$(deriveJSON defaultOptions ''ClientConfig)

optsP :: Parser Opts
optsP = Opts <$> zkOpt where
    zkOpt = optional $ strOption
        ( long "zookeeper"
       <> metavar "HOSTS"
       <> help "Zookeeper hosts" )

readZkUri :: IO ZKURI
readZkUri = do
    paths <- getAllConfigFiles "eclogues" "client.json"
    existingPath <- firstM doesFileExist paths
    case existingPath of
        Just p  -> do
            cts <- readFile p
            case eitherDecode cts of
                Right c  -> pure $ zookeeperHosts c
                Left err -> error $ "Invalid config file at " ++ p ++ ": " ++ err
        Nothing -> error "Zookeeper hosts unspecified and no config file found (try --zookeeper)"

go :: Opts -> IO ()
go opts = do
    zkUri <- maybe (readZkUri) pure $ zkUriM opts
    clientM <- (orShowError =<<) . runExceptT . ecloguesClient $ zkUri
    when (isNothing clientM) $ error "No Eclogues server advertised"
    let Just client = clientM
    ((print <=< orShowError) =<<) . runExceptT $ getJobs client

main :: IO ()
main = execParser opts >>= go where
    opts = info (helper <*> optsP)
        ( fullDesc
       <> header "eclogues-client - Direct an Eclogues task scheduler" )
