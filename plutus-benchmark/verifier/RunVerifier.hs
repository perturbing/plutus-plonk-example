{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module RunVerifier (
    runVerifier,
) where

import Offchain (compressG1Point, compressG2Point)
import Plutus.Crypto.BlsUtils (Fp (..), Fp2 (..), bls12_381_scalar_prime, mkFp, mkScalar)
import Plutus.Crypto.Plonk (
    PreInputs (..),
    PreInputsFast,
    Proof (..),
    ProofFast,
    convertToFastPreInputs,
    convertToFastProof,
 )
import Scripts (verifyPlonkFastScript, verifyPlonkScript)

import PlutusBenchmark.Common (TestSize (NoSize), printHeader, printSizeStatistics)
import qualified PlutusTx.Prelude as P
import Types (PreInputsJSONSnarkjs (..), ProofJSONSnarkjs (..))

import System.IO (Handle)
import Text.Printf (hPrintf)

import Data.Aeson (FromJSON, ToJSON, decode)
import GHC.Generics (Generic)

import Data.ByteString (pack)
import qualified Data.ByteString.Lazy as BL
import Data.Word ()

printCostsVerifierPlonkFast :: Handle -> PreInputsFast -> [Integer] -> ProofFast -> IO ()
printCostsVerifierPlonkFast h preIn pub proof =
    let script = verifyPlonkFastScript preIn pub proof
     in printSizeStatistics h NoSize script

printCostsVerifierPlonk :: Handle -> PreInputs -> [Integer] -> Proof -> IO ()
printCostsVerifierPlonk h preIn pub proof =
    let script = verifyPlonkScript preIn pub proof
     in printSizeStatistics h NoSize script

runVerifier :: Handle -> IO ()
runVerifier h = do
    jsonDataProof <- BL.readFile "test-vectors/example/proof.json"
    jsonDataPreIn <- BL.readFile "test-vectors/setup/verification_key.json"
    jsonDataPublic <- BL.readFile "test-vectors/example/public-input.json"
    let maybeProof = decode jsonDataProof :: Maybe ProofJSONSnarkjs
    let maybePreIn = decode jsonDataPreIn :: Maybe PreInputsJSONSnarkjs
    let maybePublic = fmap (map read) (decode jsonDataPublic :: Maybe [String]) :: Maybe [Integer]
    case maybeProof of
        Just proof -> case maybePreIn of
            Just preIn -> case maybePublic of
                Just public -> do
                    let p = convertProofSnarkjs proof
                    let i = convertPreInputsSnarkjs preIn
                    hPrintf h "\n\n"
                    hPrintf h $ "Run slow vanilla plonk verifier with inputs " <> show public <> "\n\n"
                    printHeader h
                    printCostsVerifierPlonk h i public p
                    let iFast = convertToFastPreInputs i
                    let pFast = convertToFastProof iFast public p
                    hPrintf h "\n\n"
                    hPrintf h $ "Run fast snarkJS plonk verifier with public inputs " <> show public <> "\n\n"
                    printHeader h
                    printCostsVerifierPlonkFast h iFast public pFast
                    hPrintf h "\n\n"
                Nothing -> print "Could not deserialize Public test vector"
            Nothing -> print "Could not deserialize PreInputs test vector"
        Nothing -> print "Could not deserialize Proof test vector"

readPointG1 :: [String] -> P.BuiltinBLS12_381_G1_Element
readPointG1 (x : y : _xs) = compressG1Point (mkFp (read x), mkFp (read y))
readPointG1 _ = error "readPointG1: empty list"

readPointG2 :: [[String]] -> P.BuiltinBLS12_381_G2_Element
readPointG2 ((a1 : a2 : _as) : (b1 : b2 : _bs) : _xys) = compressG2Point (x, y)
  where
    x :: Fp2
    x = Fp2{c0 = mkFp (read a1), c1 = mkFp (read a2)}
    y :: Fp2
    y = Fp2{c0 = mkFp (read b1), c1 = mkFp (read b2)}
readPointG2 _ = error "readPointG2: empty list"

convertProofSnarkjs :: ProofJSONSnarkjs -> Proof
convertProofSnarkjs proof =
    let f x = P.bls12_381_G1_compress $ readPointG1 x
        h x = read x :: Integer
     in Proof
            { commitmentA = f $ a proof
            , commitmentB = f $ b proof
            , commitmentC = f $ c proof
            , commitmentZ = f $ z proof
            , tLow = f $ t1 proof
            , tMid = f $ t2 proof
            , tHigh = f $ t3 proof
            , wOmega = f $ wxi proof
            , wOmegaZeta = f $ wxiw proof
            , aEval = h $ eval_a proof
            , bEval = h $ eval_b proof
            , cEval = h $ eval_c proof
            , sSig1P = h $ eval_s1 proof
            , sSig2P = h $ eval_s2 proof
            , zOmega = h $ eval_zw proof
            }

convertPreInputsSnarkjs :: PreInputsJSONSnarkjs -> PreInputs
convertPreInputsSnarkjs preIn =
    let h x = read x :: Integer
        f x = P.bls12_381_G1_compress $ readPointG1 x
     in PreInputs
            { nPublic = nPublic' preIn
            , power = power' preIn
            , k1 = mkScalar . h $ k_1' preIn
            , k2 = mkScalar . h $ k_2' preIn
            , qM = f $ qm preIn
            , qL = f $ ql preIn
            , qR = f $ qr preIn
            , qO = f $ qo preIn
            , qC = f $ qc preIn
            , sSig1 = f $ s1 preIn
            , sSig2 = f $ s2 preIn
            , sSig3 = f $ s3 preIn
            , x2 = P.bls12_381_G2_compress . readPointG2 $ x_2' preIn
            , generator = mkScalar . h $ w preIn
            }
