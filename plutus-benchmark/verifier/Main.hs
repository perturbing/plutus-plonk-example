module Main where

-- import PlutusBenchmark.BlsField.RunBlsField (runBlsField)
import RunVerifier (runVerifier)
import System.IO (stdout)

main :: IO ()
-- main = putStrLn "Hello, Haskell!"
main = do runVerifier stdout
