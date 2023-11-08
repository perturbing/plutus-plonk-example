module Main
( main
) where

import Types ( ProofJSONSnarkjs(..), PreInputsJSONSnarkjs(..) )
import Plutus.Crypto.Plonk (Proof (..), PreInputs (..), verifyPlonkSnarkjs)
import Plutus.Crypto.BlsUtils (mkScalar, compressG1Point, compressG2Point, mkFp, Fp2 (..))

import qualified PlutusTx.Prelude as P

import Data.Aeson ( decode )

import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
    jsonDataProof <- BL.readFile "test-vectors/example/proof.json"
    jsonDataPreIn <- BL.readFile "test-vectors/setup/verification_key.json"
    let maybeProof = decode jsonDataProof :: Maybe ProofJSONSnarkjs
    let maybePreIn = decode jsonDataPreIn :: Maybe PreInputsJSONSnarkjs
    case maybeProof of
        Just proof  -> case maybePreIn of
            Just preIn -> do let p = convertProofSnarkjs proof
                             let i = convertPreInputsSnarkjs preIn
                             print $ verifyPlonkSnarkjs i [20] p
            Nothing -> print "Could not deserialize PreInputs test vector"
        Nothing -> print "Could not deserialize Proof test vector"

readPointG1 :: [String] -> P.BuiltinBLS12_381_G1_Element
readPointG1 (x:y:_xs) = compressG1Point (mkFp (read x), mkFp (read y))
readPointG1 _ = error "readPointG1: empty list"

readPointG2 :: [[String]] -> P.BuiltinBLS12_381_G2_Element
readPointG2 ((a1:a2:_as):(b1:b2:_bs):_xys) = compressG2Point (x, y)
    where x :: Fp2
          x = Fp2 {c0 = mkFp (read a1), c1 = mkFp (read a2)}
          y :: Fp2 
          y = Fp2 {c0 = mkFp (read b1), c1 = mkFp (read b2)}
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
        , tLow        = f $ t1 proof
        , tMid        = f $ t2 proof
        , tHigh       = f $ t3 proof
        , wOmega      = f $ wxi proof
        , wOmegaZeta  = f $ wxiw proof
        , aEval       = h $ eval_a proof
        , bEval       = h $ eval_b proof
        , cEval       = h $ eval_c proof
        , sSig1P      = h $ eval_s1 proof
        , sSig2P      = h $ eval_s2 proof
        , zOmega      = h $ eval_zw proof
        }

convertPreInputsSnarkjs :: PreInputsJSONSnarkjs -> PreInputs
convertPreInputsSnarkjs preIn =
    let h x = read x :: Integer
    in PreInputs
    { nPublic   = nPublic' preIn
    , power     = power' preIn
    , k1        = mkScalar . h $ k_1' preIn
    , k2        = mkScalar . h $ k_2' preIn
    , qM        = readPointG1 $ qm preIn
    , qL        = readPointG1 $ ql preIn
    , qR        = readPointG1 $ qr preIn
    , qO        = readPointG1 $ qo preIn
    , qC        = readPointG1 $ qc preIn
    , sSig1     = readPointG1 $ s1 preIn
    , sSig2     = readPointG1 $ s2 preIn
    , sSig3     = readPointG1 $ s3 preIn
    , x2        = readPointG2 $ x_2' preIn
    , generator = mkScalar . h $ w preIn
    }