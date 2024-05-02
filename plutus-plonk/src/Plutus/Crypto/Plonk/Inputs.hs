{-# LANGUAGE TemplateHaskell                    #-}
{-# LANGUAGE ScopedTypeVariables                #-}
{-# LANGUAGE MultiParamTypeClasses              #-}
{-# LANGUAGE InstanceSigs                       #-}
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
{-# HLINT ignore "Use guards"                   #-}

module Plutus.Crypto.Plonk.Inputs
( Proof (..)
, PreInputs (..)
, ProofFast (..)
, PreInputsFast (..)
, convertToFastProof
, convertToFastPreInputs
) where

import Plutus.Crypto.BlsUtils
    ( Scalar (..)
    , recip
    , unScalar
    , mkScalar
    , bls12_381_scalar_prime )
import PlutusTx.Builtins
    ( BuiltinByteString,
      Integer,
      BuiltinBLS12_381_G1_Element,
      BuiltinBLS12_381_G2_Element,
      byteStringToInteger,
      integerToByteString,
      keccak_256,
      bls12_381_G1_compress )
import PlutusTx (makeLift, makeIsDataIndexed, unstableMakeIsData)
import PlutusTx.Numeric (AdditiveGroup (..), scale, (*), (+))
import PlutusTx.Prelude
    ( map
    , (.)
    , ($)
    , (<>)
    , enumFromTo
    , head
    , modulo )
import GHC.ByteOrder ( ByteOrder(..) )

import qualified Prelude as Haskell

-- Proof is a type that wraps all necesary elements needed for a proof.
-- Note that the G1 elements are compressed as bytestrings.
-- The field elements are represented as integers as they are provided 
-- by the prover and need to be checked to be in the field.
data Proof = Proof
    { commitmentA     :: BuiltinByteString -- a serialized G1 element
    , commitmentB     :: BuiltinByteString -- a serialized G1 element
    , commitmentC     :: BuiltinByteString -- a serialized G1 element
    , commitmentZ     :: BuiltinByteString -- a serialized G1 element
    , tLow            :: BuiltinByteString -- a serialized G1 element
    , tMid            :: BuiltinByteString -- a serialized G1 element
    , tHigh           :: BuiltinByteString -- a serialized G1 element
    , wOmega          :: BuiltinByteString -- a serialized G1 element
    , wOmegaZeta      :: BuiltinByteString -- a serialized G1 element
    , aEval           :: Integer           -- Field element
    , bEval           :: Integer           -- Field element
    , cEval           :: Integer           -- Field element
    , sSig1P          :: Integer           -- Field element
    , sSig2P          :: Integer           -- Field element
    , zOmega          :: Integer           -- Field element
    } deriving (Haskell.Show)

makeLift ''Proof
makeIsDataIndexed ''Proof [('Proof,0)]

-- PreInputs are the minimal values that parametrize
-- a plonk verifier. These values are known before proof
-- generation.
data PreInputs = PreInputs
    { nPublic         :: Integer                     -- number of public inputs
    , power           :: Integer                     -- power, 2^power >= number of constraints in the circuit (the upper bound used)
    , k1              :: Scalar                      -- The first field elements that creates a disjoint left coset of H
    , k2              :: Scalar                      -- The second field element that creates a disjoint left coset of H 
    , qM              :: BuiltinByteString           -- the commited polynomial of the multiplication gates
    , qL              :: BuiltinByteString           -- the commited polynomial of the left inputs of the circuit
    , qR              :: BuiltinByteString           -- the commited polynomial of the right inputs of the circuits
    , qO              :: BuiltinByteString           -- the commited polynomial of the outputs of the circuit
    , qC              :: BuiltinByteString           -- the commited polynomial of the constants inputs of the circuit
    , sSig1           :: BuiltinByteString           -- the commited polynomial of the first wire permutation
    , sSig2           :: BuiltinByteString           -- the commited polynomial of the second wire permutation
    , sSig3           :: BuiltinByteString           -- the commited polynomial of the third wire permutation
    , x2              :: BuiltinByteString           -- the first order of the SRS over G2 (g^x where x is the toxic waste / tau in power of tau)
    , generator       :: Scalar                      -- generator of the subgroup H
    } deriving (Haskell.Show)

makeLift ''PreInputs
makeIsDataIndexed ''PreInputs [('PreInputs,0)]

-- PreInputsFast are the optimised minimal values that parametrize
-- a plonk verifier. These values are known before proof
-- generation.
data PreInputsFast = PreInputsFast
    { n'               :: Integer                     -- n number of constraints (upper bound) used
    , pow'             :: Integer                     -- n = 2^pow
    , k1'              :: Scalar                      -- The first field elements that creates a disjoint left coset of H
    , k2'              :: Scalar                      -- The second field element that creates a disjoint left coset of H 
    , qM'              :: BuiltinByteString           -- the commited polynomial of the multiplication gates
    , qL'              :: BuiltinByteString           -- the commited polynomial of the left inputs of the circuit
    , qR'              :: BuiltinByteString           -- the commited polynomial of the right inputs of the circuits
    , qO'              :: BuiltinByteString           -- the commited polynomial of the outputs of the circuit
    , qC'              :: BuiltinByteString           -- the commited polynomial of the constants inputs of the circuit
    , sSig1'           :: BuiltinByteString           -- the commited polynomial of the first wire permutation
    , sSig2'           :: BuiltinByteString           -- the commited polynomial of the second wire permutation
    , sSig3'           :: BuiltinByteString           -- the commited polynomial of the third wire permutation
    , x2'              :: BuiltinByteString           -- the first order of the SRS over G2 (g^x where x is the toxic waste / tau in power of tau)
    , generators       :: [Scalar]                    -- powers of the generator of the subgroup H, [g,g^2,..,g^{number of public inputs}]
    } deriving (Haskell.Show)

makeLift ''PreInputsFast
makeIsDataIndexed ''PreInputsFast [('PreInputsFast,0)]

-- Proof is a type that wraps all necesary elements needed for a proof.
-- Note that the G1 elements are compressed as bytestrings.
-- The field elements are represented as integers as they are provided 
-- by the prover and need to be checked to be in the field.
data ProofFast = ProofFast
    { commitmentA'       :: BuiltinByteString -- a serialized G1 element
    , commitmentB'       :: BuiltinByteString -- a serialized G1 element
    , commitmentC'       :: BuiltinByteString -- a serialized G1 element
    , commitmentZ'       :: BuiltinByteString -- a serialized G1 element
    , tLow'              :: BuiltinByteString -- a serialized G1 element
    , tMid'              :: BuiltinByteString -- a serialized G1 element
    , tHigh'             :: BuiltinByteString -- a serialized G1 element
    , wOmega'            :: BuiltinByteString -- a serialized G1 element
    , wOmegaZeta'        :: BuiltinByteString -- a serialized G1 element
    , aEval'             :: Integer           -- Field element
    , bEval'             :: Integer           -- Field element
    , cEval'             :: Integer           -- Field element
    , sSig1P'            :: Integer           -- Field element
    , sSig2P'            :: Integer           -- Field element
    , zOmega'            :: Integer           -- Field element
    , lagrangeInverses   :: [Integer]         -- A list of inversed of step 6, to save onchain calc.
    } deriving (Haskell.Show)

makeLift ''ProofFast
makeIsDataIndexed ''ProofFast [('ProofFast,0)]

convertToFastPreInputs :: PreInputs -> PreInputsFast
convertToFastPreInputs preInputs@(PreInputs nPub p k1 k2 qM qL qR qO qC sSig1 sSig2 sSig3 x2 gen )
    = PreInputsFast
    { n'            = 2 Haskell.^ p
    , pow'          = p
    , k1'           = k1
    , k2'           = k2
    , qM'           = qM
    , qL'           = qL
    , qR'           = qR
    , qO'           = qO
    , qC'           = qC
    , sSig1'        = sSig1
    , sSig2'        = sSig2
    , sSig3'        = sSig3
    , x2'           = x2
    , generators    = map (`scale` gen) (enumFromTo 0 nPub)
    }

convertToFastProof :: PreInputsFast -> [Integer] -> Proof -> ProofFast
convertToFastProof preInputsFast pubInputs proof@(Proof ca cb cc cz ctl ctm cth cwo cwz ea eb ec es1 es2 ez)
    = ProofFast
    { commitmentA' = ca
    , commitmentB' = cb
    , commitmentC' = cc
    , commitmentZ' = cz
    , tLow'        = ctl
    , tMid'        = ctm
    , tHigh'       = cth
    , wOmega'      = cwo
    , wOmegaZeta'  = cwz
    , aEval'       = ea
    , bEval'       = eb
    , cEval'       = ec
    , sSig1P'      = es1
    , sSig2P'      = es2
    , zOmega'      = ez
    , lagrangeInverses = let ~betaBs = keccak_256 $ qM' preInputsFast
                                        <> qL' preInputsFast
                                        <> qR' preInputsFast
                                        <> qO' preInputsFast
                                        <> qC' preInputsFast
                                        <> sSig1' preInputsFast
                                        <> sSig2' preInputsFast
                                        <> sSig3' preInputsFast
                                        <> (integerToByteString BigEndian 32 . head) pubInputs
                                        <> ca
                                        <> cb
                                        <> cc
                             beta = mkScalar $ byteStringToInteger BigEndian  betaBs `modulo` bls12_381_scalar_prime
                             ~gammaBs = keccak_256 $ (integerToByteString BigEndian 32 . unScalar ) beta
                             gamma = mkScalar $ byteStringToInteger BigEndian  gammaBs `modulo` bls12_381_scalar_prime
                             ~alphaBs = keccak_256 $ (integerToByteString BigEndian 32 . unScalar ) beta
                                        <> (integerToByteString BigEndian 32 . unScalar ) gamma
                                        <> cz
                             alpha = mkScalar $ byteStringToInteger BigEndian alphaBs `modulo` bls12_381_scalar_prime
                             ~zetaBs = keccak_256 $ (integerToByteString BigEndian 32 . unScalar ) alpha
                                        <> ctl
                                        <> ctm
                                        <> cth
                             zeta = mkScalar $ byteStringToInteger BigEndian zetaBs `modulo` bls12_381_scalar_prime
                         in map (unScalar . recip . (\x -> mkScalar (n' preInputsFast) * (zeta - x))) $ generators preInputsFast
    }