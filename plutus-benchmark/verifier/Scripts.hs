{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Scripts
( verifyPlonkFastScript
) where

import PlutusTx (compile, getPlcNoAnn, liftCodeDef, unsafeApplyCode)
import PlutusTx.Prelude (Integer, ($))

import PlutusCore (DefaultFun, DefaultUni)
import UntypedPlutusCore qualified as UPLC

import Plutus.Crypto.Plonk (PreInputsFast, ProofFast, verifyPlonkFastSnarkjs)

verifyPlonkFastScript :: PreInputsFast -> [Integer] -> ProofFast -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
verifyPlonkFastScript preIn pub proof =
    getPlcNoAnn $ $$(compile [|| verifyPlonkFastSnarkjs ||])
       `unsafeApplyCode` liftCodeDef preIn
       `unsafeApplyCode` liftCodeDef pub
       `unsafeApplyCode` liftCodeDef proof