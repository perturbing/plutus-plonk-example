{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ViewPatterns           #-}

module Plutus.Crypto.Plonk.Inputs 
( Proof (..)
, PreInputs (..)
, ProofFast (..)
, PreInputsFast (..)
, convertToFastProof
, convertToFastPreInputs
) where

import Plutus.Crypto.BlsUtils (Scalar (..), recip, unScalar, mkScalar)
import PlutusTx.Builtins (BuiltinByteString, Integer, BuiltinBLS12_381_G1_Element, BuiltinBLS12_381_G2_Element, byteStringToInteger, blake2b_256)
import PlutusTx (makeLift, makeIsDataIndexed, unstableMakeIsData)
import PlutusTx.Numeric (AdditiveGroup (..), scale, (*))
import PlutusTx.Prelude (map, (.), ($), bls12_381_G1_uncompress, enumFromTo, takeByteString, (<>))

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
    , qM              :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the multiplication gates 
    , qL              :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the left inputs of the circuit
    , qR              :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the right inputs of the circuits
    , qO              :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the outputs of the circuit
    , qC              :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the constants inputs of the circuit
    , sSig1           :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the first wire permutation 
    , sSig2           :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the second wire permutation
    , sSig3           :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the third wire permutation
    , x2              :: BuiltinBLS12_381_G2_Element -- the first order of the SRS over G2 (g^x where x is the toxic waste / tau in power of tau)
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
    , qM'              :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the multiplication gates 
    , qL'              :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the left inputs of the circuit
    , qR'              :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the right inputs of the circuits
    , qO'              :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the outputs of the circuit
    , qC'              :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the constants inputs of the circuit
    , sSig1'           :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the first wire permutation 
    , sSig2'           :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the second wire permutation
    , sSig3'           :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the third wire permutation
    , x2'              :: BuiltinBLS12_381_G2_Element -- the first order of the SRS over G2 (g^x where x is the toxic waste / tau in power of tau)
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
    , generators    = map (`scale` gen) (enumFromTo 1 nPub)
    } 

convertToFastProof :: PreInputsFast -> Proof -> ProofFast
convertToFastProof preInputsFast proof@(Proof ca cb cc cz ctl ctm cth cwo cwz ea eb ec es1 es2 ez)
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
    , lagrangeInverses = let transcript0 = "FS transcriptdom-septesting the provercommitment a" <> ca <> "commitment b" 
                                                                                                <> cb <> "commitment c" 
                                                                                                <> cc <> "beta"
                             beta = Scalar . byteStringToInteger . takeByteString 31 . blake2b_256 $ transcript0
                             transcript1 = transcript0 <> "gamma"
                             gamma = Scalar . byteStringToInteger . takeByteString 31 . blake2b_256 $ transcript1
                             transcript2 = transcript1 <> "Permutation polynomial" <> cz <> "alpha"
                             alpha = Scalar . byteStringToInteger . takeByteString 31 . blake2b_256 $ transcript2
                             transcript3 = transcript2 <> "Quotient low polynomial" <> ctl 
                                                       <> "Quotient mid polynomial" <> ctm 
                                                       <> "Quotient high polynomial" <> cth 
                                                       <> "zeta"
                             zeta = Scalar . byteStringToInteger . takeByteString 31 . blake2b_256 $ transcript3
                         in map (unScalar . recip . (\x -> mkScalar (n' preInputsFast) * (zeta - x))) $ generators preInputsFast 
    }