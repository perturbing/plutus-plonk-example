module Offchain
( compressG1Point
, compressG2Point
) where

import qualified Prelude as Haskell
import PlutusTx.Prelude
    ( otherwise,
      Integer,
      ($),
      (&&),
      error,
      modulo,
      Eq(..),
      AdditiveGroup(..),
      AdditiveMonoid(..),
      AdditiveSemigroup(..),
      Module(..),
      MultiplicativeMonoid(..),
      MultiplicativeSemigroup(..),
      Ord((<), (<=)),
      dropByteString,
      (<>),
      even,
      divide, 
      foldr,
      Bool (..),
      (.),
      (>),
      (||),
      not )
import PlutusTx (makeLift, makeIsDataIndexed, unstableMakeIsData)
import PlutusTx.Numeric
    ( AdditiveGroup(..)
    , AdditiveMonoid(..)
    , AdditiveSemigroup(..)
    , Module(..)
    , MultiplicativeMonoid(..)
    , MultiplicativeSemigroup(..) )
import PlutusTx.Builtins
    ( bls12_381_G1_equals
    , BuiltinBLS12_381_G1_Element
    , bls12_381_G1_add
    , bls12_381_G1_compressed_zero
    , bls12_381_G1_neg
    , bls12_381_G1_scalarMul 
    , BuiltinBLS12_381_G2_Element
    , bls12_381_G2_add
    , bls12_381_G2_scalarMul
    , bls12_381_G2_neg
    , bls12_381_G2_compressed_zero
    , BuiltinByteString
    , integerToByteString
    , byteStringToInteger
    , lengthOfByteString
    , consByteString
    , sliceByteString
    , emptyByteString
    , indexByteString
    , bls12_381_G1_uncompress
    , bls12_381_G1_compress
    , bls12_381_G2_uncompress
    , bls12_381_G2_compress )

import GHC.ByteOrder ( ByteOrder(..) )
import Plutus.Crypto.BlsUtils ( Fp(..), Fp2(..))
import PlutusTx.Numeric (negate)
import Data.Bits (testBit, setBit)

-- Helper functions (this is some legacy code that we need to refactor)

compressG1Point :: (Fp, Fp) -> BuiltinBLS12_381_G1_Element
compressG1Point (x, y)
    | x == zero && y == one = bls12_381_G1_uncompress bls12_381_G1_compressed_zero
    | otherwise             = go ((bls12_381_G1_uncompress . integerToByteString BigEndian 48 . setCompressedBit . unFp ) x) y
        where setCompressedBit x = setBit x 383
              go :: BuiltinBLS12_381_G1_Element -> Fp -> BuiltinBLS12_381_G1_Element
              go p y'
                | y' < negate y' = p
                | otherwise      = bls12_381_G1_neg p

pow :: Integer -> Integer -> Integer
pow b e
    | e < 0     = zero
    | e == 0    = 1
    | even e    = pow (b*b) (e `divide` 2)
    | otherwise = b * pow (b*b) ((e - 1) `divide` 2)

compressG2Point :: (Fp2,Fp2) -> BuiltinBLS12_381_G2_Element
compressG2Point (x, y)
    | x == zero && y == one = bls12_381_G2_uncompress bls12_381_G2_compressed_zero
    | otherwise             = go (bls12_381_G2_uncompress xBsG2) y
    where x' = unFp (c0 x) + unFp (c1 x) * pow 2 384
          xBsG2 = integerToByteString BigEndian 96 $ setBit x' 767
          go :: BuiltinBLS12_381_G2_Element -> Fp2 -> BuiltinBLS12_381_G2_Element
          go x y
            | y < negate y   = x
            | otherwise      = bls12_381_G2_neg x