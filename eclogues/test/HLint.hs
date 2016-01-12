module Main (main) where

import Paths_eclogues (getDataFileName)
import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

paths :: [String]
paths =
    [ "src"
    , "test"
    ]

arguments :: IO [String]
arguments = go <$> getDataFileName "HLint.hints"
  where
    go p = ("--hint=" ++ p) : paths

main :: IO ()
main = do
    hints <- hlint =<< arguments
    if null hints then exitSuccess else exitFailure
