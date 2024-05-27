module Offchain (
    compressG1Point,
    compressG2Point,
) where

import PlutusTx (makeIsDataIndexed, makeLift, unstableMakeIsData)
import PlutusTx.Builtins (
    BuiltinBLS12_381_G1_Element,
    BuiltinBLS12_381_G2_Element,
    BuiltinByteString,
    bls12_381_G1_compress,
    bls12_381_G1_compressed_zero,
    bls12_381_G1_neg,
    bls12_381_G1_uncompress,
    bls12_381_G2_compress,
    bls12_381_G2_compressed_zero,
    bls12_381_G2_neg,
    bls12_381_G2_uncompress,
    byteStringToInteger,
    integerToByteString,
 )
import PlutusTx.Numeric (
    AdditiveGroup (..),
    AdditiveMonoid (..),
    AdditiveSemigroup (..),
    Module (..),
    MultiplicativeMonoid (..),
    MultiplicativeSemigroup (..),
    negate,
 )
import PlutusTx.Prelude (
    Bool (..),
    Eq (..),
    Integer,
    Ord ((<), (<=)),
    divide,
    even,
    foldr,
    not,
    otherwise,
    ($),
    (&&),
    (*),
    (+),
    (.),
    (<>),
    (>),
    (||),
 )
import qualified Prelude as Haskell

import Data.Bits (setBit, testBit)
import GHC.ByteOrder (ByteOrder (..))
import Plutus.Crypto.BlsUtils (Fp (..), Fp2 (..))

-- Helper functions (this is some legacy code that we need to refactor)

compressG1Point :: (Fp, Fp) -> BuiltinBLS12_381_G1_Element
compressG1Point (x, y)
    | x == zero && y == one = bls12_381_G1_uncompress bls12_381_G1_compressed_zero
    | otherwise = go ((bls12_381_G1_uncompress . integerToByteString BigEndian 48 . setCompressedBit . unFp) x) y
  where
    setCompressedBit x = setBit x 383
    go :: BuiltinBLS12_381_G1_Element -> Fp -> BuiltinBLS12_381_G1_Element
    go p y'
        | y' < negate y' = p
        | otherwise = bls12_381_G1_neg p

pow :: Integer -> Integer -> Integer
pow b e
    | e < 0 = zero
    | e == 0 = 1
    | even e = pow (b * b) (e `divide` 2)
    | otherwise = b * pow (b * b) ((e - 1) `divide` 2)

compressG2Point :: (Fp2, Fp2) -> BuiltinBLS12_381_G2_Element
compressG2Point (x, y)
    | x == zero && y == one = bls12_381_G2_uncompress bls12_381_G2_compressed_zero
    | otherwise = go (bls12_381_G2_uncompress xBsG2) y
  where
    x' = unFp (c0 x) + unFp (c1 x) * pow 2 384
    xBsG2 = integerToByteString BigEndian 96 $ setBit x' 767
    go :: BuiltinBLS12_381_G2_Element -> Fp2 -> BuiltinBLS12_381_G2_Element
    go x y
        | y < negate y = x
        | otherwise = bls12_381_G2_neg x
