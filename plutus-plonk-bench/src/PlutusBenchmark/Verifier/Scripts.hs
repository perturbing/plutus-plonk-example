{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module PlutusBenchmark.Verifier.Scripts 
( verifyPlonkScriptSnarkjsFast
, verifyPlonkScriptSnarkjs
) where

import PlutusTx
    ( getPlcNoAnn, unsafeApplyCode, liftCodeDef, compile ) 
import PlutusTx.Prelude ( Integer, ($) )

import PlutusCore (DefaultFun, DefaultUni)
import UntypedPlutusCore qualified as UPLC

import Plutus.Crypto.Plonk (Proof, PreInputs, verifyPlonkSnarkjs, PreInputsFast, ProofFast, verifyPlonkFastSnarkjs)

verifyPlonkScriptSnarkjsFast :: PreInputsFast -> [Integer] -> ProofFast -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
verifyPlonkScriptSnarkjsFast preIn pub proof = 
    getPlcNoAnn $ $$(compile [|| verifyPlonkFastSnarkjs ||]) 
       `unsafeApplyCode` liftCodeDef preIn
       `unsafeApplyCode` liftCodeDef pub
       `unsafeApplyCode` liftCodeDef proof

verifyPlonkScriptSnarkjs :: PreInputs -> [Integer] -> Proof -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
verifyPlonkScriptSnarkjs preIn pub proof = 
    getPlcNoAnn $ $$(compile [|| verifyPlonkSnarkjs ||]) 
       `unsafeApplyCode` liftCodeDef preIn
       `unsafeApplyCode` liftCodeDef pub
       `unsafeApplyCode` liftCodeDef proof