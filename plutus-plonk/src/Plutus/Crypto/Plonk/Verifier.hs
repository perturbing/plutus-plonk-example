{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE Strict             #-}


module Plutus.Crypto.Plonk.Verifier
( verifyPlonkSnarkjs
) where

import Plutus.Crypto.Plonk.Inputs (PreInputs (..), Proof (..))
import Plutus.Crypto.BlsUtils
    ( mkScalar
    , Scalar (..)
    , MultiplicativeGroup (..)
    , powerOfTwoExponentiation
    , bls12_381_field_prime
    , reverseByteString )
import PlutusTx.Prelude
    ( Integer
    , Bool (..)
    , BuiltinByteString
    , (>)
    , otherwise
    , enumFromTo
    , (.)
    , (&&)
    , divide
    , error
    , (<)
    , (||)
    , even
    , (<>)
    , ($)
    , modulo
    , length
    , sum
    , dropByteString )
import PlutusTx.Eq (Eq (..))
import PlutusTx.List (map, zipWith, foldr, head, and)
import PlutusTx.Numeric
    ( AdditiveGroup(..),
      AdditiveMonoid(..),
      AdditiveSemigroup(..),
      Module(..),
      MultiplicativeMonoid(one),
      MultiplicativeSemigroup((*)),
      negate )
import PlutusTx.Builtins
    ( blake2b_256
    , keccak_256
    , byteStringToInteger
    , integerToByteString
    , consByteString
    , emptyByteString
    , bls12_381_G1_compress
    , bls12_381_G1_uncompress
    , bls12_381_G1_generator
    , bls12_381_G2_generator
    , bls12_381_millerLoop
    , bls12_381_finalVerify
    , lengthOfByteString
    , indexByteString
    , sliceByteString )

{-# INLINABLE exponentiate #-}
exponentiate :: Integer -> Integer -> Integer
exponentiate x n
    | n < 0 || x < 0    = error ()
    | n == 0            = 1
    | x == 0            = 0
    | even n            = exponentiate x (n `divide` 2) * exponentiate x (n `divide` 2)
    | otherwise         = x * exponentiate x ((n - 1) `divide` 2) * exponentiate x ((n - 1) `divide` 2)

{-# INLINABLE padTo32Bytes #-}
padTo32Bytes :: BuiltinByteString -> BuiltinByteString
padTo32Bytes ~bs
    | lengthOfByteString bs == 32 = bs
    | lengthOfByteString bs < 32  = padTo32Bytes (bs <> consByteString 0 emptyByteString)
    | otherwise                   = error ()

{-# INLINEABLE verifyPlonkSnarkjs #-}
verifyPlonkSnarkjs :: PreInputs -> [Integer] -> Proof -> Bool
verifyPlonkSnarkjs preInputs@(PreInputs nPub p k1 k2 qM qL qR qO qC sSig1 sSig2 sSig3 x2 gen)
            pubInputs
            proof@(Proof ca cb cc cz ctl ctm cth cwo cwz ea eb ec es1 es2 ez)
    | (bls12_381_G1_uncompress -> commA) <- ca
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
    , let (w1 : wxs) = map (negate . mkScalar) pubInputs
    =
    let n = exponentiate 2 p
        ~betaBs = keccak_256 $ bls12_381_G1_compress qM
                            <> bls12_381_G1_compress qL
                            <> bls12_381_G1_compress qR
                            <> bls12_381_G1_compress qO
                            <> bls12_381_G1_compress qC
                            <> bls12_381_G1_compress sSig1
                            <> bls12_381_G1_compress sSig2
                            <> bls12_381_G1_compress sSig3
                            <> (reverseByteString . padTo32Bytes . integerToByteString . head) pubInputs
                            <> ca
                            <> cb
                            <> cc
        beta = mkScalar $ (byteStringToInteger . reverseByteString) betaBs `modulo` bls12_381_field_prime
        ~gammaBs = keccak_256 $ (reverseByteString . padTo32Bytes . integerToByteString . unScalar ) beta
        gamma = mkScalar $ (byteStringToInteger . reverseByteString) gammaBs `modulo` bls12_381_field_prime
        ~alphaBs = keccak_256 $ (reverseByteString . padTo32Bytes . integerToByteString . unScalar ) beta
                             <> (reverseByteString . padTo32Bytes . integerToByteString . unScalar ) gamma
                             <> cz
        alpha = mkScalar $ (byteStringToInteger . reverseByteString) alphaBs `modulo` bls12_381_field_prime
        ~zetaBs = keccak_256 $ (reverseByteString . padTo32Bytes . integerToByteString . unScalar ) alpha
                             <> ctl
                             <> ctm
                             <> cth
        zeta = mkScalar $ (byteStringToInteger . reverseByteString) zetaBs `modulo` bls12_381_field_prime
        ~vBs = keccak_256 $ (reverseByteString . padTo32Bytes . integerToByteString . unScalar ) zeta
                            <> (reverseByteString . padTo32Bytes . integerToByteString) ea
                            <> (reverseByteString . padTo32Bytes . integerToByteString) eb
                            <> (reverseByteString . padTo32Bytes . integerToByteString) ec
                            <> (reverseByteString . padTo32Bytes . integerToByteString) es1
                            <> (reverseByteString . padTo32Bytes . integerToByteString) es2
                            <> (reverseByteString . padTo32Bytes . integerToByteString) ez
        v = mkScalar $ (byteStringToInteger . reverseByteString) vBs `modulo` bls12_381_field_prime
        ~uBs = keccak_256 $ cwo <> cwz
        u = mkScalar $ (byteStringToInteger . reverseByteString) uBs `modulo` bls12_381_field_prime
        -- -- this is Z_H(zeta) in the plonk paper
        zeroPoly = scale n zeta - one
        -- this is L_1(zeta) and the higher order L_i
        (lagrangePoly1 : lagrangePolyXs) = map (\i -> (scale i gen * zeroPoly) * recip (mkScalar n * (zeta - scale i gen))) (enumFromTo 0 nPub)
        -- this is PI(zeta) in the plonk paper
        piZeta = w1 * lagrangePoly1 + sum (zipWith (*) wxs lagrangePolyXs)
        -- this is r_0 in the plonk paper
        r0 = piZeta - lagrangePoly1 * alpha * alpha - alpha * (evalA + beta*evalS1 + gamma) * (evalB + beta*evalS2 + gamma) * (evalC + gamma) * evalZOmega
        -- this is [D]_1 in the plonk paper
        batchPolyCommitG1 = scale (evalA*evalB) qM
                          + scale evalA qL
                          + scale evalB qR
                          + scale evalC qO 
                          + qC
                          + scale ((evalA + beta*zeta + gamma)*(evalB +beta*k1*zeta + gamma)*(evalC + beta*k2*zeta + gamma)*alpha + lagrangePoly1*alpha * alpha + u) commZ
                          - scale ((evalA +beta*evalS1+gamma)*(evalB + beta*evalS2+gamma)*alpha*beta*evalZOmega) sSig3
                          - scale zeroPoly (commTLow + scale (scale n zeta) commTMid + scale (scale (2*n) zeta) commTHigh)
        -- this is [F]_1 in the plonk paper
        batchPolyCommitFull = batchPolyCommitG1 + scale v (commA + scale v (commB + scale v (commC + scale v (sSig1 + scale v sSig2))))
        -- this is [E]_1 in the plonk paper
        groupEncodedBatchEval = scale (negate r0 + v * (evalA + v * (evalB + v * (evalC + v * (evalS1 + v * evalS2)))) + u*evalZOmega ) bls12_381_G1_generator
    in
    -- the final check that under the pairing.
    bls12_381_finalVerify 
      (bls12_381_millerLoop (commWOmega + scale u commWOmegaZeta) x2) 
      (bls12_381_millerLoop (scale zeta commWOmega + scale (u*zeta*gen) commWOmegaZeta + batchPolyCommitFull - groupEncodedBatchEval) bls12_381_G2_generator)