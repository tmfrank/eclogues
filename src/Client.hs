{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (readFile)

import Eclogues.Util (orShowError)
import Eclogues.Client (EcloguesClient (..), ecloguesClient)
import Eclogues.TaskSpec (Name)
import Eclogues.Zookeeper (ZKURI)

import Control.Applicative ((<$>), (<*>), pure, optional)
import Control.Monad ((<=<), when)
import Control.Monad.Loops (firstM)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson (eitherDecode)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString.Lazy (readFile)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Data.Text.Lazy (pack)
import Options.Applicative ( Parser, strOption, strArgument, long, metavar, help
                           , subparser, command, progDesc
                           , execParser, info, helper, fullDesc, header )
import System.Directory (doesFileExist)
import System.Environment.XDG.BaseDir (getAllConfigFiles)

data Opts = Opts { optZkUri  :: Maybe ZKURI
                 , optCommand :: Command }

data Command = ListJobs
             | JobState Name
             | CreateJob FilePath

data ClientConfig = ClientConfig { zookeeperHosts :: ZKURI }

$(deriveJSON defaultOptions ''ClientConfig)

readZkUri :: IO ZKURI
readZkUri = do
    paths <- getAllConfigFiles "eclogues" "client.json"
    existingPath <- firstM doesFileExist paths
    case existingPath of
        Just p  -> do
            cts <- readFile p
            case eitherDecode cts of
                Right c   -> pure $ zookeeperHosts c
                Left  err -> error $ "Invalid config file at " ++ p ++ ": " ++ err
        Nothing -> error "Zookeeper hosts unspecified and no config file found (try --zookeeper)"

go :: Opts -> IO ()
go opts = do
    zkUri <- maybe (readZkUri) pure $ optZkUri opts
    clientM <- (orShowError =<<) . runExceptT . ecloguesClient $ zkUri
    when (isNothing clientM) $ error "No Eclogues server advertised"
    let Just client = clientM
    case optCommand opts of
        ListJobs      -> runClient $ getJobs client
        JobState name -> runClient $ getJobState client name
        CreateJob sf  -> do
            cts <- readFile sf
            case eitherDecode cts of
                Right spec -> (orShowError =<<) . runExceptT $ createJob client spec
                Left  err  -> error $ "Invalid spec file: " ++ err
    where
        runClient :: forall a e. (Show a, Show e) => ExceptT e IO a -> IO ()
        runClient = ((print <=< orShowError) =<<) . runExceptT

main :: IO ()
main = execParser opts >>= go where
    opts = info (helper <*> optsP)
        ( fullDesc
       <> header "eclogues-client - Direct an Eclogues task scheduler" )
    optsP :: Parser Opts
    optsP = Opts <$> zkOpt <*> cmdOpt
    zkOpt = optional $ strOption
        ( long "zookeeper"
       <> metavar "HOSTS"
       <> help "Zookeeper hosts" )
    cmdOpt :: Parser Command
    cmdOpt = subparser
        ( command "list"   (info (pure ListJobs)         (progDesc "List all jobs"))
       <> command "state"  (info (JobState  <$> nameArg) (progDesc "Get the state of a job"))
       <> command "create" (info (CreateJob <$> specArg) (progDesc "Schedule a job")) )
    nameArg = pack <$> strArgument (metavar "JOB_NAME")
    specArg = strArgument (metavar "SPEC_FILE")
