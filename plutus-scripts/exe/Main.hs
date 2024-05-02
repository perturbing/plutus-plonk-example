{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE BinaryLiterals       #-}
 
module Main where

import Cardano.Api           
  ( writeFileTextEnvelope
  , PlutusScriptVersion (..)
  , PlutusScriptV1
  , PlutusScriptV2
  , PlutusScriptV3
  , IsPlutusScriptLanguage (..)
  , Script (..)
  , ScriptHash (..)
  , hashScript
  , prettyPrintJSON)
import Cardano.Api.Shelley   
  ( File (..)
  , PlutusScript (..)
  , Script (..)
  , serialiseToRawBytes
  , fromPlutusData
  , scriptDataToJsonDetailedSchema
  , unsafeHashableScriptData)
import PlutusTx               (CompiledCode, liftCodeDef, unsafeApplyCode)
import qualified PlutusTx.Builtins  as PlutusTx
import qualified PlutusLedgerApi.V3 as PlutusV3
import qualified PlutusLedgerApi.V2 as PlutusV2
import qualified PlutusLedgerApi.V1 as PlutusV1

import Plutus.Crypto.BlsUtils ( mkScalar, Fp(..), mkFp, Fp2 (..) )
import Offchain (compressG1Point, compressG2Point)
import Plutus.Crypto.Plonk 
    ( Proof (..)
    , PreInputs (..)
    , verifyPlonkSnarkjs
    , ProofFast (..)
    , PreInputsFast(..)
    , convertToFastProof
    , convertToFastPreInputs
    , verifyPlonkFastSnarkjs )

import Types ( ProofJSONSnarkjs(..), PreInputsJSONSnarkjs(..) )

import Scripts                
  ( alwaysTrueMintCode
  , zkMintingScriptCode )

import GHC.ByteOrder ( ByteOrder(..) )
import qualified Data.ByteString.Char8 as BS8
import qualified PlutusTx.Prelude as P
import Data.Aeson ( decode, Value )
import qualified Data.ByteString.Lazy as BL

writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = writeFileTextEnvelope (File filePath) Nothing script >>= \case
  Left err -> print "error writing script"
  Right () -> putStrLn $ "Serialized script to: " ++ filePath

writeCodeToFile :: forall lang a. PlutusScriptVersion lang -> FilePath -> CompiledCode a -> IO ()
writeCodeToFile version filePath = case version of
  PlutusScriptV1 -> writePlutusScriptToFile @PlutusScriptV1 filePath . PlutusScriptSerialised . PlutusV1.serialiseCompiledCode
  PlutusScriptV2 -> writePlutusScriptToFile @PlutusScriptV2 filePath . PlutusScriptSerialised . PlutusV2.serialiseCompiledCode
  PlutusScriptV3 -> writePlutusScriptToFile @PlutusScriptV3 filePath . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode

---------------------------------------

scriptHashAlwaysTrueMint :: ScriptHash
scriptHashAlwaysTrueMint = hashScript . PlutusScript PlutusScriptV3 . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode $ alwaysTrueMintCode

alwaysTrueCurrencySymbol :: PlutusV3.CurrencySymbol
alwaysTrueCurrencySymbol = PlutusV3.CurrencySymbol . PlutusV3.toBuiltin . serialiseToRawBytes $ scriptHashAlwaysTrueMint

dataToJSON :: PlutusV3.ToData a => a -> Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . PlutusV3.toData

printDataToJSON :: PlutusV3.ToData a => a -> IO ()
printDataToJSON = putStrLn . BS8.unpack . prettyPrintJSON . dataToJSON

main :: IO ()
main = do 
  jsonDataProof <- BL.readFile "test-vectors/example/proof.json"
  jsonDataPreIn <- BL.readFile "test-vectors/setup/verification_key.json"
  let maybeProof = decode jsonDataProof :: Maybe ProofJSONSnarkjs
  let maybePreIn = decode jsonDataPreIn :: Maybe PreInputsJSONSnarkjs
  writeCodeToFile PlutusScriptV3 "./assets/V3/alwaysTrueMint.plutus" alwaysTrueMintCode
  case maybePreIn of
    Just preIn -> do let i = convertPreInputsSnarkjs preIn
                         iFast = convertToFastPreInputs i
                         zkMintingScriptCodeApplied = zkMintingScriptCode `unsafeApplyCode` liftCodeDef (PlutusV3.toBuiltinData iFast)
                     writeCodeToFile PlutusScriptV3 "./assets/V3/zkMintingScript.plutus" zkMintingScriptCodeApplied
                     case maybeProof of
                      Just proof -> do let p = convertProofSnarkjs proof
                                           pFast = convertToFastProof iFast [20] p
                                           redeemer = (20 :: Integer, pFast)
                                       writeFile "./assets/redeemers/mintRedeemer.json" (BS8.unpack . prettyPrintJSON $ dataToJSON redeemer)
                      Nothing -> print "Could not deserialize Proof test vector"
    Nothing -> print "Could not deserialize PreInputs test vector"

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
        f x = P.bls12_381_G1_compress $ readPointG1 x
    in PreInputs
    { nPublic   = nPublic' preIn
    , power     = power' preIn
    , k1        = mkScalar . h $ k_1' preIn
    , k2        = mkScalar . h $ k_2' preIn
    , qM        = f $ qm preIn
    , qL        = f $ ql preIn
    , qR        = f $ qr preIn
    , qO        = f $ qo preIn
    , qC        = f $ qc preIn
    , sSig1     = f $ s1 preIn
    , sSig2     = f $ s2 preIn
    , sSig3     = f $ s3 preIn
    , x2        = P.bls12_381_G2_compress . readPointG2 $ x_2' preIn
    , generator = mkScalar . h $ w preIn
    }