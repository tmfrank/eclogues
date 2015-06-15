{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (readFile)

import Database.Zookeeper.ManagedEvents (ZKURI, withZookeeper)
import Eclogues.Util (orShowError)
import Eclogues.Client (EcloguesClient (..), ecloguesClient)
import Eclogues.TaskSpec (Name)

import Control.Applicative ((<$>), (<*>), pure, optional)
import Control.Monad ((<=<), when)
import Control.Monad.Loops (firstM)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson (encode, eitherDecode)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString.Lazy (readFile)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Data.Text.Lazy (pack)
import Database.Zookeeper (ZLogLevel (ZLogWarn), setDebugLevel)
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
             | GetMaster
             | GetHealth

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
    setDebugLevel ZLogWarn
    clientM <- withZookeeper zkUri $ (orShowError =<<) . runExceptT . ecloguesClient
    when (isNothing clientM) $ error "No Eclogues server advertised"
    let Just client = clientM
    case optCommand opts of
        GetMaster     -> let (h,p) = masterHost client in putStrLn (h ++ ':':show p)
        GetHealth     -> runClient (BSLC.putStrLn . encode) $ getHealth client
        ListJobs      -> runClient print $ getJobs client
        JobState name -> runClient print $ getJobState client name
        CreateJob sf  -> do
            cts <- readFile sf
            case eitherDecode cts of
                Right spec -> (orShowError =<<) . runExceptT $ createJob client spec
                Left  err  -> error $ "Invalid spec file: " ++ err
    where
        runClient :: forall a e. (Show e) => (a -> IO ()) -> ExceptT e IO a -> IO ()
        runClient f = ((f <=< orShowError) =<<) . runExceptT

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
       <> command "create" (info (CreateJob <$> specArg) (progDesc "Schedule a job"))
       <> command "master" (info (pure GetMaster)        (progDesc "Print Eclogues master host"))
       <> command "health" (info (pure GetHealth)        (progDesc "Get service health")) )
    nameArg = pack <$> strArgument (metavar "JOB_NAME")
    specArg = strArgument (metavar "SPEC_FILE")
