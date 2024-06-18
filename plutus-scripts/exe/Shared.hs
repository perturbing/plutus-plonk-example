{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Shared where

import PlutusLedgerApi.V3 (
    ScriptContext (..),
    UnsafeFromData (..),
 )
import PlutusTx (
    CompiledCode,
    compile,
    makeIsDataIndexed,
 )
import PlutusTx.Builtins (
    BuiltinData,
    error,
 )
import PlutusTx.Prelude (
    Bool (..),
    BuiltinUnit,
    check,
    ($),
 )

-- Helper function to wrap a script to error on the return of a False.
{-# INLINEABLE wrapOneArg #-}
wrapOneArg ::
    (UnsafeFromData a) =>
    (a -> Bool) ->
    (BuiltinData -> BuiltinUnit)
wrapOneArg f ctx =
    check
        $ f
            (unsafeFromBuiltinData ctx)

{-# INLINEABLE wrapTwoArgs #-}
wrapTwoArgs ::
    (UnsafeFromData a) =>
    (a -> ScriptContext -> Bool) ->
    (BuiltinData -> BuiltinData -> BuiltinUnit)
wrapTwoArgs f a ctx =
    check
        $ f
            (unsafeFromBuiltinData a)
            (unsafeFromBuiltinData ctx)

{-# INLINEABLE wrapThreeArgs #-}
wrapThreeArgs ::
    ( UnsafeFromData a
    , UnsafeFromData b
    ) =>
    (a -> b -> ScriptContext -> Bool) ->
    (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
wrapThreeArgs f a b ctx =
    check
        $ f
            (unsafeFromBuiltinData a)
            (unsafeFromBuiltinData b)
            (unsafeFromBuiltinData ctx)

{-# INLINEABLE wrapFourArgs #-}
wrapFourArgs ::
    ( UnsafeFromData a
    , UnsafeFromData b
    , UnsafeFromData c
    ) =>
    (a -> b -> c -> ScriptContext -> Bool) ->
    (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
wrapFourArgs f a b c ctx =
    check
        $ f
            (unsafeFromBuiltinData a)
            (unsafeFromBuiltinData b)
            (unsafeFromBuiltinData c)
            (unsafeFromBuiltinData ctx)
