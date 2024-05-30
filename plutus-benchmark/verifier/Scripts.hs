{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Scripts (
    verifyPlonkFastScript,
    verifyPlonkScript,
) where

import PlutusTx (compile, getPlcNoAnn, liftCodeDef, unsafeApplyCode)
import PlutusTx.Prelude (Integer, ($))

import PlutusCore (DefaultFun, DefaultUni)
import qualified UntypedPlutusCore as UPLC

import Plutus.Crypto.Plonk (PreInputs, PreInputsFast, Proof, ProofFast, verifyPlonkFastSnarkjs, verifyPlonkSnarkjs)

verifyPlonkFastScript :: PreInputsFast -> [Integer] -> ProofFast -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
verifyPlonkFastScript preIn pub proof =
    getPlcNoAnn
        $ $$(compile [||verifyPlonkFastSnarkjs||])
        `unsafeApplyCode` liftCodeDef preIn
        `unsafeApplyCode` liftCodeDef pub
        `unsafeApplyCode` liftCodeDef proof

verifyPlonkScript :: PreInputs -> [Integer] -> Proof -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
verifyPlonkScript preIn pub proof =
    getPlcNoAnn
        $ $$(compile [||verifyPlonkSnarkjs||])
        `unsafeApplyCode` liftCodeDef preIn
        `unsafeApplyCode` liftCodeDef pub
        `unsafeApplyCode` liftCodeDef proof
