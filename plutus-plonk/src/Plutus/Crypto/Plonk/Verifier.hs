{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
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
{-# HLINT ignore "Use guards"                   #-}

module Plutus.Crypto.Plonk.Verifier (
    verifyPlonkSnarkjs,
    verifyPlonkFastSnarkjs,
) where

import GHC.ByteOrder (ByteOrder (..))
import Plutus.Crypto.BlsUtils (
    MultiplicativeGroup (..),
    Scalar (..),
    bls12_381_scalar_prime,
    mkScalar,
    negateScalar,
    powerOfTwoExponentiationScalar,
 )
import Plutus.Crypto.Plonk.Inputs (
    PreInputs (..),
    PreInputsFast (..),
    Proof (..),
    ProofFast (..),
 )
import PlutusTx.Builtins (
    BuiltinBLS12_381_G1_Element,
    BuiltinByteString,
    Integer,
    blake2b_224,
    bls12_381_G1_compress,
    bls12_381_G1_compressed_generator,
    bls12_381_G1_uncompress,
    bls12_381_G2_compress,
    bls12_381_G2_compressed_generator,
    bls12_381_G2_uncompress,
    bls12_381_finalVerify,
    bls12_381_millerLoop,
    byteStringToInteger,
    integerToByteString,
 )
import PlutusTx.Eq (Eq (..))
import PlutusTx.List (and, head, map, tail, zipWith)
import PlutusTx.Numeric (
    AdditiveGroup (..),
    AdditiveMonoid (..),
    AdditiveSemigroup (..),
    Module (..),
    MultiplicativeMonoid (..),
    MultiplicativeSemigroup (..),
 )
import PlutusTx.Prelude (
    Bool (..),
    divide,
    enumFromTo,
    error,
    even,
    mconcat,
    modulo,
    otherwise,
    sum,
    ($),
    (&&),
    (.),
    (<),
    (<>),
    (>),
    (||),
 )

{-# INLINEABLE exponentiate #-}
exponentiate :: Integer -> Integer -> Integer
exponentiate x n
    | n < 0 || x < 0 = error ()
    | n == 0 = 1
    | x == 0 = 0
    | even n = exponentiate x (n `divide` 2) * exponentiate x (n `divide` 2)
    | otherwise = x * exponentiate x ((n - 1) `divide` 2) * exponentiate x ((n - 1) `divide` 2)

{-# INLINEABLE verifyPlonkSnarkjs #-}
verifyPlonkSnarkjs :: PreInputs -> [Integer] -> Proof -> Bool
verifyPlonkSnarkjs
    preInputs@(PreInputs nPub p k1 k2 qMBs qLBs qRBs qOBs qCBs sSig1Bs sSig2Bs sSig3Bs x2Bs gen)
    pubInputs
    proof@(Proof ca cb cc cz ctl ctm cth cwo cwz ea eb ec es1 es2 ez)
        | (bls12_381_G1_uncompress -> qM) <- qMBs
        , (bls12_381_G1_uncompress -> qL) <- qLBs
        , (bls12_381_G1_uncompress -> qR) <- qRBs
        , (bls12_381_G1_uncompress -> qO) <- qOBs
        , (bls12_381_G1_uncompress -> qC) <- qCBs
        , (bls12_381_G1_uncompress -> sSig1) <- sSig1Bs
        , (bls12_381_G1_uncompress -> sSig2) <- sSig2Bs
        , (bls12_381_G1_uncompress -> sSig3) <- sSig3Bs
        , (bls12_381_G2_uncompress -> x2) <- x2Bs
        , (bls12_381_G1_uncompress -> commA) <- ca
        , (bls12_381_G1_uncompress -> commB) <- cb
        , (bls12_381_G1_uncompress -> commC) <- cc
        , (bls12_381_G1_uncompress -> commZ) <- cz
        , (bls12_381_G1_uncompress -> commTLow) <- ctl
        , (bls12_381_G1_uncompress -> commTMid) <- ctm
        , (bls12_381_G1_uncompress -> commTHigh) <- cth
        , (bls12_381_G1_uncompress -> commWOmega) <- cwo
        , (bls12_381_G1_uncompress -> commWOmegaZeta) <- cwz
        , (mkScalar -> evalA) <- ea
        , (mkScalar -> evalB) <- eb
        , (mkScalar -> evalC) <- ec
        , (mkScalar -> evalS1) <- es1
        , (mkScalar -> evalS2) <- es2
        , (mkScalar -> evalZOmega) <- ez
        , let (w1 : wxs) = map (negateScalar . mkScalar) pubInputs
        , let bls12_381_G1_generator = bls12_381_G1_uncompress bls12_381_G1_compressed_generator
        , let bls12_381_G2_generator = bls12_381_G2_uncompress bls12_381_G2_compressed_generator =
            let n = exponentiate 2 p
                betaBs =
                    blake2b_224
                        $ qMBs
                        <> qLBs
                        <> qRBs
                        <> qOBs
                        <> qCBs
                        <> sSig1Bs
                        <> sSig2Bs
                        <> sSig3Bs
                        <> mconcat (map (integerToByteString BigEndian 32) pubInputs)
                        <> ca
                        <> cb
                        <> cc
                beta = Scalar $ byteStringToInteger BigEndian betaBs
                gammaBs = blake2b_224 $ (integerToByteString BigEndian 32 . unScalar) beta
                gamma = Scalar $ byteStringToInteger BigEndian gammaBs
                alphaBs =
                    blake2b_224
                        $ (integerToByteString BigEndian 32 . unScalar) beta
                        <> (integerToByteString BigEndian 32 . unScalar) gamma
                        <> cz
                alpha = Scalar $ byteStringToInteger BigEndian alphaBs
                zetaBs =
                    blake2b_224
                        $ (integerToByteString BigEndian 32 . unScalar) alpha
                        <> ctl
                        <> ctm
                        <> cth
                zeta = Scalar $ byteStringToInteger BigEndian zetaBs
                vBs =
                    blake2b_224
                        $ (integerToByteString BigEndian 32 . unScalar) zeta
                        <> integerToByteString BigEndian 32 ea
                        <> integerToByteString BigEndian 32 eb
                        <> integerToByteString BigEndian 32 ec
                        <> integerToByteString BigEndian 32 es1
                        <> integerToByteString BigEndian 32 es2
                        <> integerToByteString BigEndian 32 ez
                v = Scalar $ byteStringToInteger BigEndian vBs
                uBs = blake2b_224 $ cwo <> cwz
                u = Scalar $ byteStringToInteger BigEndian uBs
                -- -- this is Z_H(zeta) in the plonk paper
                zeroPoly = scale n zeta - one
                -- this is L_1(zeta) and the higher order L_i
                (lagrangePoly1 : lagrangePolyXs) = map (\i -> (scale i gen * zeroPoly) * recip (mkScalar n * (zeta - scale i gen))) (enumFromTo 0 nPub)
                -- this is PI(zeta) in the plonk paper
                piZeta = w1 * lagrangePoly1 + sum (zipWith (*) wxs lagrangePolyXs)
                -- this is r_0 in the plonk paper
                r0 = piZeta - lagrangePoly1 * alpha * alpha - alpha * (evalA + beta * evalS1 + gamma) * (evalB + beta * evalS2 + gamma) * (evalC + gamma) * evalZOmega
                -- this is [D]_1 in the plonk paper
                batchPolyCommitG1 =
                    scale (evalA * evalB) qM
                        + scale evalA qL
                        + scale evalB qR
                        + scale evalC qO
                        + qC
                        + scale ((evalA + beta * zeta + gamma) * (evalB + beta * k1 * zeta + gamma) * (evalC + beta * k2 * zeta + gamma) * alpha + lagrangePoly1 * alpha * alpha + u) commZ
                        - scale ((evalA + beta * evalS1 + gamma) * (evalB + beta * evalS2 + gamma) * alpha * beta * evalZOmega) sSig3
                        - scale zeroPoly (commTLow + scale (scale n zeta) commTMid + scale (scale (2 * n) zeta) commTHigh)
                -- this is [F]_1 in the plonk paper
                batchPolyCommitFull = batchPolyCommitG1 + scale v (commA + scale v (commB + scale v (commC + scale v (sSig1 + scale v sSig2))))
                -- this is [E]_1 in the plonk paper
                groupEncodedBatchEval = scale (negateScalar r0 + v * (evalA + v * (evalB + v * (evalC + v * (evalS1 + v * evalS2)))) + u * evalZOmega) bls12_381_G1_generator
             in -- the final check that under the pairing.
                bls12_381_finalVerify
                    (bls12_381_millerLoop (commWOmega + scale u commWOmegaZeta) x2)
                    (bls12_381_millerLoop (scale zeta commWOmega + scale (u * zeta * gen) commWOmegaZeta + batchPolyCommitFull - groupEncodedBatchEval) bls12_381_G2_generator)

-- a general vanilla plonk verifier optimized.
{-# INLINEABLE verifyPlonkFastSnarkjs #-}
verifyPlonkFastSnarkjs :: PreInputsFast -> [Integer] -> ProofFast -> Bool
verifyPlonkFastSnarkjs
    preInputsFast@(PreInputsFast n p k1 k2 qMBs qLBs qRBs qOBs qCBs sSig1Bs sSig2Bs sSig3Bs x2Bs gens)
    pubInputs
    proofFast@(ProofFast ca cb cc cz ctl ctm cth cwo cwz ea eb ec es1 es2 ez lagInv)
        | (bls12_381_G1_uncompress -> qM) <- qMBs
        , (bls12_381_G1_uncompress -> qL) <- qLBs
        , (bls12_381_G1_uncompress -> qR) <- qRBs
        , (bls12_381_G1_uncompress -> qO) <- qOBs
        , (bls12_381_G1_uncompress -> qC) <- qCBs
        , (bls12_381_G1_uncompress -> sSig1) <- sSig1Bs
        , (bls12_381_G1_uncompress -> sSig2) <- sSig2Bs
        , (bls12_381_G1_uncompress -> sSig3) <- sSig3Bs
        , (bls12_381_G2_uncompress -> x2) <- x2Bs
        , (bls12_381_G1_uncompress -> commA) <- ca
        , (bls12_381_G1_uncompress -> commB) <- cb
        , (bls12_381_G1_uncompress -> commC) <- cc
        , (bls12_381_G1_uncompress -> commZ) <- cz
        , (bls12_381_G1_uncompress -> commTLow) <- ctl
        , (bls12_381_G1_uncompress -> commTMid) <- ctm
        , (bls12_381_G1_uncompress -> commTHigh) <- cth
        , (bls12_381_G1_uncompress -> commWOmega) <- cwo
        , (bls12_381_G1_uncompress -> commWOmegaZeta) <- cwz
        , (mkScalar -> evalA) <- ea
        , (mkScalar -> evalB) <- eb
        , (mkScalar -> evalC) <- ec
        , (mkScalar -> evalS1) <- es1
        , (mkScalar -> evalS2) <- es2
        , (mkScalar -> evalZOmega) <- ez
        , let (w1 : wxs) = map (negateScalar . mkScalar) pubInputs
        , let lagsInv = map mkScalar lagInv
        , let bls12_381_G1_generator = bls12_381_G1_uncompress bls12_381_G1_compressed_generator
        , let bls12_381_G2_generator = bls12_381_G2_uncompress bls12_381_G2_compressed_generator =
            let betaBs =
                    blake2b_224
                        $ qMBs
                        <> qLBs
                        <> qRBs
                        <> qOBs
                        <> qCBs
                        <> sSig1Bs
                        <> sSig2Bs
                        <> sSig3Bs
                        <> mconcat (map (integerToByteString BigEndian 32) pubInputs)
                        <> ca
                        <> cb
                        <> cc
                beta = Scalar $ byteStringToInteger BigEndian betaBs
                gammaBs = blake2b_224 $ (integerToByteString BigEndian 32 . unScalar) beta
                gamma = Scalar $ byteStringToInteger BigEndian gammaBs
                alphaBs =
                    blake2b_224
                        $ (integerToByteString BigEndian 32 . unScalar) beta
                        <> (integerToByteString BigEndian 32 . unScalar) gamma
                        <> cz
                alpha = Scalar $ byteStringToInteger BigEndian alphaBs
                zetaBs =
                    blake2b_224
                        $ (integerToByteString BigEndian 32 . unScalar) alpha
                        <> ctl
                        <> ctm
                        <> cth
                zeta = Scalar $ byteStringToInteger BigEndian zetaBs
                vBs =
                    blake2b_224
                        $ (integerToByteString BigEndian 32 . unScalar) zeta
                        <> integerToByteString BigEndian 32 ea
                        <> integerToByteString BigEndian 32 eb
                        <> integerToByteString BigEndian 32 ec
                        <> integerToByteString BigEndian 32 es1
                        <> integerToByteString BigEndian 32 es2
                        <> integerToByteString BigEndian 32 ez
                v = Scalar $ byteStringToInteger BigEndian vBs
                uBs = blake2b_224 $ cwo <> cwz
                u = Scalar $ byteStringToInteger BigEndian uBs
                powOfTwoZetaP = powerOfTwoExponentiationScalar zeta p
                powOfTwoZetaPMinOne = powOfTwoZetaP - one
                (lagrangePoly1 : lagrangePolyXs) = zipWith (\x y -> x * powOfTwoZetaPMinOne * y) gens lagsInv
                piZeta = w1 * lagrangePoly1 + sum (zipWith (*) wxs lagrangePolyXs)
                alphaSquare = alpha * alpha
                alphaEvalZOmega = alpha * evalZOmega
                betaZeta = beta * zeta
                evalAPlusGamma = evalA + gamma
                evalBPlusGamma = evalB + gamma
                evalCPlusGamma = evalC + gamma
                betaEvalS1 = beta * evalS1
                betaEvalS2 = beta * evalS2
                r0 = piZeta - lagrangePoly1 * alphaSquare - alphaEvalZOmega * (evalAPlusGamma + betaEvalS1) * (evalBPlusGamma + betaEvalS2) * evalCPlusGamma
                batchPolyCommitG1 =
                    scale (evalA * evalB) qM
                        + scale evalA qL
                        + scale evalB qR
                        + scale evalC qO
                        + qC
                        + scale ((evalAPlusGamma + betaZeta) * (evalBPlusGamma + betaZeta * k1) * (evalCPlusGamma + betaZeta * k2) * alpha + lagrangePoly1 * alphaSquare + u) commZ
                        - scale ((evalAPlusGamma + betaEvalS1) * (evalBPlusGamma + betaEvalS2) * alphaEvalZOmega * beta) sSig3
                        - scale powOfTwoZetaPMinOne (commTLow + scale powOfTwoZetaP commTMid + scale (powerOfTwoExponentiationScalar powOfTwoZetaP 1) commTHigh)
                batchPolyCommitFull = batchPolyCommitG1 + scale v (commA + scale v (commB + scale v (commC + scale v (sSig1 + scale v sSig2))))
                groupEncodedBatchEval = scale (negateScalar r0 + v * (evalA + v * (evalB + v * (evalC + v * (evalS1 + v * evalS2)))) + u * evalZOmega) bls12_381_G1_generator
             in bls12_381_finalVerify
                    (bls12_381_millerLoop (commWOmega + scale u commWOmegaZeta) x2)
                    (bls12_381_millerLoop (scale zeta commWOmega + scale (u * zeta * head (tail gens)) commWOmegaZeta + batchPolyCommitFull - groupEncodedBatchEval) bls12_381_G2_generator)
                    && and (zipWith (\x y -> x * Scalar n * (zeta - y) == one) lagsInv gens)
