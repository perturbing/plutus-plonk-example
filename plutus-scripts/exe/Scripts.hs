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
    ( (.),
      ($),
      length,
      even,
      divide,
      Ord (..),
      Eq (..), 
      Maybe (..), 
      maybe, 
      (<$>), 
      check,
      traceIfFalse,
      (/=), 
      modulo )
import PlutusTx.List
    ( any,
      map,
      elem,
      foldr,
      filter, 
      find )
import PlutusTx.Builtins
    ( BuiltinByteString,
      Integer,
      error, 
      BuiltinData, 
      bls12_381_G1_uncompress,
      bls12_381_G2_uncompress, 
      bls12_381_G1_compressed_generator,
      bls12_381_G2_compressed_generator,
      bls12_381_millerLoop,
      bls12_381_finalVerify)
import PlutusTx.AssocMap (member)
import PlutusLedgerApi.V3
    ( TxOutRef(..),
      ScriptContext(..),
      CurrencySymbol,
      TxInfo(..),
      TxInInfo (..),
      ScriptPurpose (..),
      TxOut (..),
      Value (..),
      PubKeyHash,
      TxCert (..), 
      ToData (..), 
      Datum (..), 
      UnsafeFromData (..),
      OutputDatum (..))
import PlutusLedgerApi.V3.Contexts (
    findTxInByTxOutRef,
    txSignedBy )
import PlutusTx
    ( compile,
      CompiledCode,
      makeIsDataIndexed )
import PlutusTx.Bool
    ( Bool(..),
      (&&), otherwise)
import PlutusTx.Numeric
    ( AdditiveGroup(..),
      AdditiveSemigroup (..) )
import Shared (wrapFourArgs, wrapThreeArgs, wrapTwoArgs)

-- [bls accumulator minting script]
-- The idea of this script is to mint tokens under its policyID if and only if
-- that assets has a token name that is not minted yet. 
-- To achieve this, we use a pairing based subtractive accumulator over the bls pairing curve.
-- This requires a trusted setup (also know as a trapdoor value or common reference string).
-- Moreover, since this is a statebased solution, the minting script only validates true
-- if and only if a hardcoded policy ID of an asset (from now on State NFT),
-- is in one of the spending inputs.
-- 
-- The locking address of this hardcode State NFT has in its datum the latest accumulator.
-- This locking script is parametarized by the above minting script, so that for each asset that 
-- is being minted by that class, it checks that the asset name (modulo the bls scalar prime)
-- 

-- [Main Minting Script]
-- This script just checks that the hard-coded currency symbol of the State NFT is 
-- in any spending input of the transaction.
{-# INLINABLE mainMintingScript #-}
mainMintingScript :: CurrencySymbol -> BuiltinData -> ScriptContext -> Bool
mainMintingScript symbol _ ctx =  case scriptContextPurpose ctx of
    Minting _  -> any (\value -> symbol `member` value) txInputsValues
    _          -> False
    where
        -- The list of transaction inputs being consumed in this transaction.
        txInputs = txInfoInputs . scriptContextTxInfo $ ctx
        -- The list of value maps of the transaction inputs.
        txInputsValues = map (getValue . txOutValue . txInInfoResolved) txInputs

{-# INLINABLE mkWrappedMainMintingScript #-}
mkWrappedMainMintingScript :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedMainMintingScript = wrapThreeArgs mainMintingScript

mainMintingScriptCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
mainMintingScriptCode = $$(compile [|| mkWrappedMainMintingScript ||])

-- for an removal we need to check also two things
-- 1) given datum f(s), verify that f(s)=(x-s)*proof(s)
-- 2) new datum is f'(s)=f(s)*inverse(g^(bs'mod'p -s))
-- here 1 checks that x is a member, and 2) removes it
{-# INLINABLE stateNFTLockScript #-}
stateNFTLockScript :: CurrencySymbol -> BuiltinByteString -> BuiltinByteString -> ScriptContext -> Bool
stateNFTLockScript symbol acc pi ctx = case scriptContextPurpose ctx of
    Spending txOurRef -> checkMember 
        where
            checkMember = bls12_381_finalVerify
                            (bls12_381_millerLoop 
                                (bls12_381_G1_uncompress acc)
                                (bls12_381_G2_uncompress bls12_381_G2_compressed_generator)
                            )
                            (bls12_381_millerLoop
                                (bls12_381_G1_uncompress bls12_381_G1_compressed_generator)
                                (bls12_381_G2_uncompress pi)
                            )
    _                 -> False

{-# INLINABLE wrappedStateNFTLockScript #-}
wrappedStateNFTLockScript :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedStateNFTLockScript = wrapFourArgs stateNFTLockScript

stateNFTLockScriptCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
stateNFTLockScriptCode = $$(compile [|| wrappedStateNFTLockScript ||])

-- testing purposes

{-# INLINABLE alwaysTrueMint #-}
alwaysTrueMint :: BuiltinData -> ScriptContext -> Bool
alwaysTrueMint _ _ = True

{-# INLINABLE wrappedAlwaysTrueMint #-}
wrappedAlwaysTrueMint :: BuiltinData -> BuiltinData -> ()
wrappedAlwaysTrueMint = wrapTwoArgs alwaysTrueMint

alwaysTrueMintCode :: CompiledCode (BuiltinData -> BuiltinData -> ())
alwaysTrueMintCode = $$(compile [|| wrappedAlwaysTrueMint ||])