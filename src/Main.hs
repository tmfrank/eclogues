{-# LANGUAGE OverloadedStrings #-}

module Main where

import AuroraAPI (thriftClient, createJob)
import TaskSpec (Resources (..), TaskSpec (..))
import Units

import Control.Exception (bracket)
import Network.URI (parseURI)

fromRight :: (Show e) => Either e a -> a
fromRight (Right a) = a
fromRight (Left e)  = error $ "fromRight: " ++ show e

main :: IO ()
main = do
    let (Just uri) = parseURI "http://192.168.100.3:8081/api"
    client <- thriftClient uri

    --print =<< getJobs client
    let task = TaskSpec "hello2" "/bin/echo hello" (Resources (mega byte 10) (mebi byte 10) (core 0.1))
    --print $ auroraJobConfig task

    print =<< createJob client task
    --bracket
    --    (fromRight <$> acquireLock client "hello1")
    --    (\lock -> releaseLock client lock)
    --    (\lock -> print =<< createJob client task lock)
    return ()
