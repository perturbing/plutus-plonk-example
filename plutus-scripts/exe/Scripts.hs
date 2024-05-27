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
    ScriptContext (..),
    ScriptPurpose (..),
    scriptContextPurpose,
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
import PlutusTx.Prelude (
    ($),
    (.),
    (==),
 )
import Shared (wrapFourArgs, wrapThreeArgs, wrapTwoArgs)

{-# INLINEABLE zkMintingScript #-}
zkMintingScript :: PreInputsFast -> ([Integer], ProofFast) -> ScriptContext -> Bool
zkMintingScript preIn (pubIn, proof) ctx = case scriptContextPurpose ctx of
    Minting _ -> verifyPlonkFastSnarkjs preIn pubIn proof
    _ -> False

{-# INLINEABLE mkWrappedZkMintingScript #-}
mkWrappedZkMintingScript :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedZkMintingScript = wrapThreeArgs zkMintingScript

zkMintingScriptCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
zkMintingScriptCode = $$(compile [||mkWrappedZkMintingScript||])

-- testing purposes

{-# INLINEABLE alwaysTrueMint #-}
alwaysTrueMint :: BuiltinData -> ScriptContext -> Bool
alwaysTrueMint _ _ = True

{-# INLINEABLE wrappedAlwaysTrueMint #-}
wrappedAlwaysTrueMint :: BuiltinData -> BuiltinData -> ()
wrappedAlwaysTrueMint = wrapTwoArgs alwaysTrueMint

alwaysTrueMintCode :: CompiledCode (BuiltinData -> BuiltinData -> ())
alwaysTrueMintCode = $$(compile [||wrappedAlwaysTrueMint||])
