{-# LANGUAGE LambdaCase #-}

-- | Miscellaneous shared code for benchmarking-related things.
module PlutusBenchmark.Common (
    TestSize (..),
    printHeader,
    printSizeStatistics,
)
where

-- ### CAUTION! ###.  Changing the number and/or order of the exports here may
-- change the execution times of the validation benchmarks.  See
-- https://github.com/IntersectMBO/plutus/issues/5906.

import PlutusBenchmark.ProtocolParameters as PP

import PlutusCore qualified as PLC
import PlutusCore.Default
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (..))
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults qualified as PLC
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (..), ExMemory (..))

import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek as Cek

import Data.ByteString qualified as BS
import Data.SatInt (fromSatInt)
import Flat qualified
import System.IO
import Text.Printf (hPrintf, printf)

type Term = UPLC.Term PLC.NamedDeBruijn DefaultUni DefaultFun ()

-- | Remove the textual names from a NamedDeBruijn term
toAnonDeBruijnTerm ::
    Term ->
    UPLC.Term UPLC.DeBruijn DefaultUni DefaultFun ()
toAnonDeBruijnTerm = UPLC.termMapNames UPLC.unNameDeBruijn

toAnonDeBruijnProg ::
    UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun () ->
    UPLC.Program UPLC.DeBruijn DefaultUni DefaultFun ()
toAnonDeBruijnProg (UPLC.Program () ver body) =
    UPLC.Program () ver $ toAnonDeBruijnTerm body

-- | Evaluate a script and return the CPU and memory costs (according to the cost model)
getCostsCek :: UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun () -> (Integer, Integer)
getCostsCek (UPLC.Program _ _ prog) =
    case Cek.runCekDeBruijn PLC.defaultCekParametersForTesting Cek.tallying Cek.noEmitter prog of
        (_res, Cek.TallyingSt _ budget, _logs) ->
            let ExBudget (ExCPU cpu) (ExMemory mem) = budget
             in (fromSatInt cpu, fromSatInt mem)

---------------- Printing tables of information about costs ----------------

data TestSize
    = NoSize
    | TestSize Integer

stringOfTestSize :: TestSize -> String
stringOfTestSize =
    \case
        NoSize -> "-"
        TestSize n -> show n

-- Printing utilities
percentage :: (Integral a, Integral b) => a -> b -> Double
percentage a b =
    let a' = fromIntegral a :: Double
        b' = fromIntegral b :: Double
     in (a' * 100) / b'

percentTxt :: (Integral a, Integral b) => a -> b -> String
percentTxt a b = printf "(%.1f%%)" (percentage a b)

-- | Print a header to be followed by a list of size statistics.
printHeader :: Handle -> IO ()
printHeader h = do
    hPrintf h "    n     Script size             CPU usage               Memory usage\n"
    hPrintf h "  ----------------------------------------------------------------------\n"

{- | Evaluate a script and print out the serialised size and the CPU and memory
usage, both as absolute values and percentages of the maxima specified in the
protocol parameters.
-}
printSizeStatistics ::
    Handle ->
    TestSize ->
    UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun () ->
    IO ()
printSizeStatistics h n script = do
    let serialised = Flat.flat (UPLC.UnrestrictedProgram $ toAnonDeBruijnProg script)
        size = BS.length serialised
        (cpu, mem) = getCostsCek script
    hPrintf
        h
        "  %3s %7d %8s %15d %8s %15d %8s \n"
        (stringOfTestSize n)
        size
        (percentTxt size PP.maxTxSize)
        cpu
        (percentTxt cpu PP.maxTxExSteps)
        mem
        (percentTxt mem PP.maxTxExMem)
