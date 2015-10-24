{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : portable

Entry point for the Eclogues CLI.
-}

module Main where

import Prelude hiding (readFile)

import Database.Zookeeper.ManagedEvents (ZKURI, withZookeeper)
import Eclogues.Util (orShowError)
import Eclogues.Client ( ecloguesClient, getJobs, getJobStage, masterHost
                       , createJob, getHealth )
import qualified Eclogues.Job as Job

import Control.Applicative (optional)
import Control.Lens ((^.))
import Control.Monad ((<=<), when)
import Control.Monad.Loops (firstM)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson (encode, eitherDecode)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Bifoldable (biList)
import Data.Bifunctor (second)
import Data.ByteString.Lazy (readFile)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.List (foldl', intercalate)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Database.Zookeeper (ZLogLevel (ZLogWarn), setDebugLevel)
import Options.Applicative ( Parser, argument, strOption, strArgument, switch
                           , long, metavar, help, info, helper, fullDesc
                           , subparser, command, progDesc, header, execParser )
import Options.Applicative.Types (readerAsk)
import System.Directory (doesFileExist)
import System.Environment.XDG.BaseDir (getAllConfigFiles)

-- | CLI options.
data Opts = Opts { optZkUri   :: Maybe ZKURI
                 , optCommand :: Command }

-- | CLI subcommand.
data Command = ListJobs
             | JobStage Job.Name
             | CreateJob FilePath
             | GetMaster
             | GetHealth
             | JobStats Bool

-- | JSON configuration type.
data ClientConfig = ClientConfig { zookeeperHosts :: ZKURI }

$(deriveJSON defaultOptions ''ClientConfig)

-- | Find and read the config file, or 'error'.
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
    zkUri <- maybe readZkUri pure $ optZkUri opts
    setDebugLevel ZLogWarn
    clientM <- withZookeeper zkUri $ (orShowError =<<) . runExceptT . ecloguesClient
    when (isNothing clientM) $ error "No Eclogues server advertised"
    let Just client = clientM
    case optCommand opts of
        GetMaster     -> let (h,p) = masterHost client in putStrLn (h ++ ':':show p)
        GetHealth     -> runClient (BSLC.putStrLn . encode) $ getHealth client
        ListJobs      -> runClient print $ getJobs client
        JobStats as   -> runClient (printStats as) $ getJobs client
        JobStage name -> runClient print $ getJobStage client name
        CreateJob sf  -> do
            cts <- readFile sf
            case eitherDecode cts of
                Right spec -> (orShowError =<<) . runExceptT $ createJob client spec
                Left  err  -> error $ "Invalid spec file: " ++ err
    where
        runClient :: forall a e. (Show e) => (a -> IO ()) -> ExceptT e IO a -> IO ()
        runClient f = ((f <=< orShowError) =<<) . runExceptT
        printStats :: Bool -> [Job.Status] -> IO ()
        printStats as = mapM_ (putStrLn . intercalate "\t" . biList . second show) . HashMap.toList . getStats (statStart as)
        statStart :: Bool -> HashMap String Integer
        statStart False = HashMap.empty
        statStart True  = foldl' (flip (`HashMap.insert` 0)) HashMap.empty Job.majorStages
        getStats :: HashMap String Integer -> [Job.Status] -> HashMap String Integer
        getStats = foldl' (\m j -> HashMap.insertWith (+) (Job.majorStage $ j ^. Job.stage) 1 m)

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
       <> command "stage"  (info (JobStage  <$> nameArg) (progDesc "Get the stage of a job"))
       <> command "create" (info (CreateJob <$> specArg) (progDesc "Schedule a job"))
       <> command "master" (info (pure GetMaster)        (progDesc "Print Eclogues master host"))
       <> command "stats"  (info (JobStats <$> allArg)   (progDesc "Get running job stats"))
       <> command "health" (info (pure GetHealth)        (progDesc "Get service health")) )
    nameArg = argument (Job.mkName . T.pack =<< readerAsk) (metavar "JOB_NAME")
    specArg = strArgument (metavar "SPEC_FILE")
    allArg  = switch (long "all-stages" <> help "List stages with no associated jobs")
