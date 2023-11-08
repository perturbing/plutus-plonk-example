module Main where

import PlutusBenchmark.Verifier.RunVerifier (runVerifier)
import System.IO (stdout)

main :: IO ()
main = do runVerifier stdout