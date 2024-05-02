{-# LANGUAGE TemplateHaskell                    #-}
{-# LANGUAGE NamedFieldPuns                     #-}
{-# LANGUAGE DataKinds                          #-}
{-# LANGUAGE NoImplicitPrelude                  #-}
{-# LANGUAGE ViewPatterns                       #-}
{-# LANGUAGE Strict                             #-}
{-# LANGUAGE OverloadedStrings                  #-}

{-# OPTIONS_GHC -fno-ignore-interface-pragmas   #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas     #-}
{-# OPTIONS_GHC -fno-full-laziness              #-}
{-# OPTIONS_GHC -fno-spec-constr                #-}
{-# OPTIONS_GHC -fno-specialise                 #-}
{-# OPTIONS_GHC -fno-strictness                 #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields        #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields  #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas       #-}
{-# HLINT ignore "Use null"                     #-}

module Scripts where

import PlutusTx.Prelude
    ( (.)
    , ($)
    , (==)
    )
import PlutusTx.Builtins
    ( BuiltinByteString
    , Integer
    , error
    , BuiltinData )
import PlutusLedgerApi.V3
    ( ScriptContext(..)
    , ScriptPurpose (..)
    , scriptContextPurpose)
import PlutusTx
    ( compile
    , CompiledCode
    , makeIsDataIndexed )
import PlutusTx.Bool
    ( Bool(..)
    ,  (&&)
    , otherwise )
import Shared (wrapFourArgs, wrapThreeArgs, wrapTwoArgs)
import Plutus.Crypto.Plonk (ProofFast (..), PreInputsFast (..), verifyPlonkFastSnarkjs)

{-# INLINABLE zkMintingScript #-}
zkMintingScript :: PreInputsFast -> (Integer,ProofFast) -> ScriptContext -> Bool
zkMintingScript preIn (pubIn,proof) ctx = case scriptContextPurpose ctx of
    Minting _  -> verifyPlonkFastSnarkjs preIn [pubIn] proof
    _          -> False

{-# INLINABLE mkWrappedZkMintingScript #-}
mkWrappedZkMintingScript :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedZkMintingScript = wrapThreeArgs zkMintingScript

zkMintingScriptCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
zkMintingScriptCode = $$(compile [|| mkWrappedZkMintingScript ||])

-- testing purposes

{-# INLINABLE alwaysTrueMint #-}
alwaysTrueMint :: BuiltinData -> ScriptContext -> Bool
alwaysTrueMint _ _ = True

{-# INLINABLE wrappedAlwaysTrueMint #-}
wrappedAlwaysTrueMint :: BuiltinData -> BuiltinData -> ()
wrappedAlwaysTrueMint = wrapTwoArgs alwaysTrueMint

alwaysTrueMintCode :: CompiledCode (BuiltinData -> BuiltinData -> ())
alwaysTrueMintCode = $$(compile [|| wrappedAlwaysTrueMint ||])