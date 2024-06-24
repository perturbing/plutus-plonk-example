{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}

{-# HLINT ignore "Use null"                     #-}

module Scripts where

import Plutus.Crypto.Plonk (PreInputsFast (..), ProofFast (..), verifyPlonkFastSnarkjs)
import PlutusLedgerApi.V3 (
    Redeemer (..),
    ScriptContext (..),
    ScriptInfo (..),
    ScriptPurpose (..),
    fromBuiltinData,
 )
import PlutusTx (
    CompiledCode,
    compile,
    makeIsDataIndexed,
 )
import PlutusTx.Bool (
    Bool (..),
    otherwise,
    (&&),
 )
import PlutusTx.Builtins (
    BuiltinByteString,
    BuiltinData,
    Integer,
    error,
 )
import PlutusTx.Builtins.Internal as BI
import PlutusTx.Prelude (
    BuiltinUnit,
    Maybe (..),
    ($),
    (.),
    (==),
 )
import Shared (wrapFourArgs, wrapOneArg, wrapThreeArgs, wrapTwoArgs)

{-# INLINEABLE zkMintingScript #-}
zkMintingScript :: PreInputsFast -> ScriptContext -> Bool
zkMintingScript preIn ctx = case scriptContextScriptInfo ctx of
    MintingScript _ -> case redeemer of
        Just (pubIn, proof) -> verifyPlonkFastSnarkjs preIn pubIn proof
        Nothing -> False
      where
        redeemer = fromBuiltinData . getRedeemer $ scriptContextRedeemer ctx :: Maybe ([Integer], ProofFast)
    _ -> False

{-# INLINEABLE mkWrappedZkMintingScript #-}
mkWrappedZkMintingScript :: BuiltinData -> BuiltinData -> BuiltinUnit
mkWrappedZkMintingScript = wrapTwoArgs zkMintingScript

zkMintingScriptCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
zkMintingScriptCode = $$(compile [||mkWrappedZkMintingScript||])

-- testing purposes

{-# INLINEABLE alwaysTrueMint #-}
alwaysTrueMint :: BuiltinData -> Bool
alwaysTrueMint _ = True

{-# INLINEABLE wrappedAlwaysTrueMint #-}
wrappedAlwaysTrueMint :: BuiltinData -> BuiltinUnit
wrappedAlwaysTrueMint _ = BI.unitval

alwaysTrueMintCode :: CompiledCode (BuiltinData -> BuiltinUnit)
alwaysTrueMintCode = $$(compile [||wrappedAlwaysTrueMint||])
