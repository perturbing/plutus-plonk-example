{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE Strict                 #-}

module Plutus.Crypto.BlsUtils
( bls12_381_base_prime
, Fp
, unFp
, mkFp
, compressG1Point
, unCompressG1Point
, Fp2 (..)
, compressG2Point
, unCompressG2Point
, bls12_381_field_prime
, Scalar (..)
, mkScalar
, MultiplicativeGroup (..)
, pow
, powMod
, modularExponentiationScalar
, modularExponentiationFp
, modularExponentiationFp2
, powerOfTwoExponentiation
, reverseByteString
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
    ( AdditiveGroup(..),
      AdditiveMonoid(..),
      AdditiveSemigroup(..),
      Module(..),
      MultiplicativeMonoid(..),
      MultiplicativeSemigroup(..), negate )
import PlutusTx.Builtins
    ( bls12_381_G1_equals,
      BuiltinBLS12_381_G1_Element,
      bls12_381_G1_add,
      bls12_381_G1_zero,
      bls12_381_G1_neg,
      bls12_381_G1_scalarMul,
      BuiltinBLS12_381_G2_Element,
      bls12_381_G2_add,
      bls12_381_G2_scalarMul,
      bls12_381_G2_neg,
      bls12_381_G2_zero,
      BuiltinByteString,
      integerToByteString,
      byteStringToInteger,
      writeBitByteString,
      shiftByteString,
      popCountByteString,
      lengthOfByteString,
      xorByteString,
      testBitByteString,
      consByteString,
      sliceByteString,
      emptyByteString,
      indexByteString,
      bls12_381_G1_uncompress,
      bls12_381_G1_compress,
      bls12_381_G2_uncompress,
      bls12_381_G2_compress )

-- In this module, we setup the two prime order fields for BLS12-381.
-- as the type Fp (base points) and Scalar. 
-- Note that for safety, both the Scalar and Fp constructors
-- are not exposed. Instead, the mkScalar and mkFp suffice, 
-- which fail in a script if an integer provided that is negative.

-- The prime order of the generator in the field. So, g^order = id,
bls12_381_field_prime :: Integer
bls12_381_field_prime = 52435875175126190479447740508185965837690552500527637822603658699938581184513

-- The prime of the base field. So for a g on the curve, its 
-- x and y coordinates are elements of the base field.
bls12_381_base_prime :: Integer
bls12_381_base_prime = 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787

newtype Scalar = Scalar { unScalar :: Integer} deriving (Haskell.Show)
makeLift ''Scalar
makeIsDataIndexed ''Scalar [('Scalar,0)]

-- Exclude for safety negative integers and integers large/equal
-- to the field prime. This is the primary interface to work with
-- the Scalar type onchain. This is for security reasons 
-- (to make sure they are field elements).
{-# INLINABLE mkScalar #-}
mkScalar :: Integer -> Scalar
mkScalar n | 0 <= n && n < bls12_381_field_prime = Scalar n
           | otherwise                           = error ()

instance Eq Scalar where
    {-# INLINABLE (==) #-}
    Scalar a == Scalar b = a == b

instance AdditiveSemigroup Scalar where
    {-# INLINABLE (+) #-}
    (+) (Scalar a) (Scalar b) = Scalar $ (a+b) `modulo` bls12_381_field_prime

instance AdditiveMonoid Scalar where
    {-# INLINABLE zero #-}
    zero = Scalar 0

instance AdditiveGroup Scalar where
    {-# INLINABLE (-) #-}
    (-) (Scalar a) (Scalar b) = Scalar $ (a-b) `modulo` bls12_381_field_prime

instance MultiplicativeSemigroup Scalar where
    {-# INLINABLE (*) #-}
    (*) (Scalar a) (Scalar b) = Scalar $ (a*b) `modulo` bls12_381_field_prime

instance MultiplicativeMonoid Scalar where
    {-# INLINABLE one #-}
    one = Scalar 1

-- In plutus 1.9, PlutusTx.Numeric does not implement a Multiplicative group.
-- But since we use a field, inversion is well-defined if we exclude 0.
-- We also implement the reciprocal (the multiplicative inverse of an element in the group).
-- For the additive group, there is negate function in PlutusTx.Numeric.
class MultiplicativeMonoid a => MultiplicativeGroup a where
    div :: a -> a -> a
    recip :: a -> a

-- Modular exponentiation by squaring. This assumes that the exponent is
-- a big endian bytestring. Note that integegerToByteString is little endian.
{-# INLINABLE modularExponentiationScalar #-}
modularExponentiationScalar :: Scalar -> BuiltinByteString -> Scalar
modularExponentiationScalar b ~e
    | e == emptyByteString = one
    | otherwise            = t * modularExponentiationScalar (b*b) (sliceByteString 1 (lengthOfByteString e) e)
        where t = if testBitByteString e 0 then b else one

-- Reverse a builtin byte string of arbitrary length
-- This can convert between little and big endian.
{-# INLINABLE reverseByteString #-}
reverseByteString :: BuiltinByteString -> BuiltinByteString
reverseByteString ~bs
    | lengthOfByteString bs == 0 = bs
    | otherwise                  = reverseByteString (sliceByteString 1 (lengthOfByteString bs) bs) <> sliceByteString 0 1 bs

-- this one costs around 12.1% of cpu budget
-- while bitshifts modExp cost around 9.6%
{-# INLINABLE powMod #-}
powMod :: Scalar -> Integer -> Scalar
powMod b e
    | e < 0     = zero
    | e == 0    = one
    | even e   = powMod (b*b) (e `divide` 2)
    | otherwise = b * powMod (b*b) ((e - 1) `divide` 2)

-- In math this is b^a mod p, where b is of type scalar and a any integer
-- note that there is still some overhead here due to the conversion from
-- little endian to big endian (and bs <-> integer). This can be
-- optimized in the future.
instance Module Integer Scalar where
    {-# INLINABLE scale #-}
    scale :: Integer -> Scalar -> Scalar
    scale e b = powMod b e --modularExponentiationScalar b (reverseByteString (integerToByteString e)) -- powMod b a is also a correct implementation

instance MultiplicativeGroup Scalar where
    {-# INLINABLE div #-}
    div a b | b == Scalar 0 = error ()
            | otherwise     = a * scale (bls12_381_field_prime - 2) b -- use Fermat little theorem
    {-# INLINABLE recip #-}
    recip = div one

-- This is a special case of modular exponentiation, where the exponent is a power of two.
-- This saves alot of script budget. Note that for x^e,  e = 2^k, and k is used below
{-# INLINABLE powerOfTwoExponentiation #-}
powerOfTwoExponentiation :: Scalar -> Integer -> Scalar
powerOfTwoExponentiation x k = if k < 0 then error () else go x k
    where go x' k'
            | k' == 0    = x'
            | otherwise = powerOfTwoExponentiation (x'*x') (k' - 1)

-- The field elements are the x and y coordinates of the points on the curve.
newtype Fp = Fp { unFp :: Integer} deriving (Haskell.Show)
makeLift ''Fp
makeIsDataIndexed ''Fp [('Fp ,0)]

{-# INLINABLE mkFp #-}
mkFp :: Integer -> Fp
mkFp n | 0 <= n && n < bls12_381_base_prime = Fp n
       | otherwise                          = error ()

instance Eq Fp where
    {-# INLINABLE (==) #-}
    Fp a == Fp b = a == b

instance AdditiveSemigroup Fp where
    {-# INLINABLE (+) #-}
    (+) (Fp a) (Fp b) = Fp $ (a+b) `modulo` bls12_381_base_prime

instance AdditiveMonoid Fp where
    {-# INLINABLE zero #-}
    zero = Fp 0

instance AdditiveGroup Fp where
    {-# INLINABLE (-) #-}
    (-) (Fp a) (Fp b) = Fp $ (a-b) `modulo` bls12_381_base_prime

instance MultiplicativeSemigroup Fp where
    {-# INLINABLE (*) #-}
    (*) (Fp a) (Fp b) = Fp $ (a*b) `modulo` bls12_381_base_prime

instance MultiplicativeMonoid Fp where
    {-# INLINABLE one #-}
    one = Fp 1

{-# INLINABLE modularExponentiationFp #-}
modularExponentiationFp :: Fp -> BuiltinByteString -> Fp
modularExponentiationFp b e
    | popCountByteString e == 0  = one
    | otherwise = t * modularExponentiationFp (b*b) (shiftByteString e (-1))
                where t = if testBitByteString e 0 then b else one

instance Module Integer Fp where
    {-# INLINABLE scale #-}
    scale :: Integer -> Fp -> Fp
    scale a b = modularExponentiationFp b (reverseByteString (integerToByteString a))

instance MultiplicativeGroup Fp where
    {-# INLINABLE div #-}
    div a b | b == Fp 0     = error ()
            | otherwise     = a * scale (bls12_381_base_prime - 2) b -- use Fermat little theorem
    {-# INLINABLE recip #-}
    recip = div one

instance Ord Fp where
    {-# INLINABLE (<) #-}
    (<) :: Fp -> Fp -> Bool
    Fp a < Fp b = a < b
    {-# INLINABLE (<=) #-}
    Fp a <= Fp b = a <= b
    {-# INLINABLE (>) #-}
    Fp a > Fp b = a > b
    -- {-# INLINABLE (>=) #-}
    -- Fp a >= Fp b = a >= b


{-# INLINABLE compressG1Point #-}
compressG1Point :: (Fp, Fp) -> BuiltinBLS12_381_G1_Element
compressG1Point (x, y)
    | x == zero && y == one = bls12_381_G1_zero
    | otherwise             = go ((setCompressedBit . fixLengthBs . integerToByteString. unFp) x) y
        where fixLengthBs :: BuiltinByteString -> BuiltinByteString
              fixLengthBs bs
                | lengthOfByteString bs == 48 = bs
                | otherwise                   = fixLengthBs (bs <> consByteString 0 emptyByteString)
              setCompressedBit x = bls12_381_G1_uncompress $ reverseByteString (writeBitByteString x 7 True)
              go :: BuiltinBLS12_381_G1_Element -> Fp -> BuiltinBLS12_381_G1_Element
              go p y'
                | y' < negate y' = p
                | otherwise      = bls12_381_G1_neg p

{-# INLINABLE unCompressG1Point #-}
unCompressG1Point :: BuiltinBLS12_381_G1_Element -> (Fp, Fp)
unCompressG1Point p
    | p == bls12_381_G1_zero = (zero, one)
    | otherwise              = (x, y')
        where p' = reverseByteString . bls12_381_G1_compress $ p
              thirdBit = testBitByteString p' 5
              x = Fp . byteStringToInteger $ foldr (\i acc -> writeBitByteString acc i False) p' [7,6,5]
              y = scale ((bls12_381_base_prime + 1) `divide` 4) (x * x * x + Fp 4)
              y' = if (thirdBit && y < negate y) || (not thirdBit && y > negate y) then negate y else y


-- The field elements are the x and y coordinates of the points on the curve.
data Fp2 = Fp2
    { c0 :: Fp
    , c1 :: Fp
    } deriving (Haskell.Show)
makeLift ''Fp2
makeIsDataIndexed ''Fp2 [('Fp2 ,0)]

instance Eq Fp2 where
    {-# INLINABLE (==) #-}
    Fp2 x1 y1 == Fp2 x2 y2 = x1 == x2 && y1 == y2

instance AdditiveSemigroup Fp2 where
    {-# INLINABLE (+) #-}
    (+) (Fp2 a b) (Fp2 c d) = Fp2 (a+c) (b+d)

instance AdditiveMonoid Fp2 where
    {-# INLINABLE zero #-}
    zero = Fp2 zero zero

instance AdditiveGroup Fp2 where
    {-# INLINABLE (-) #-}
    (-) (Fp2 a b) (Fp2 c d) = Fp2 (a-c) (b-d)

instance MultiplicativeSemigroup Fp2 where
    {-# INLINABLE (*) #-}
    (*) (Fp2 a b) (Fp2 c d) = Fp2 (a*c - b*d) (a*d + b*c)

instance MultiplicativeMonoid Fp2 where
    {-# INLINABLE one #-}
    one = Fp2 one zero

{-# INLINABLE pow #-}
pow :: Integer -> Integer -> Integer
pow b e
    | e < 0     = zero
    | e == 0    = 1
    | even e    = pow (b*b) (e `divide` 2)
    | otherwise = b * pow (b*b) ((e - 1) `divide` 2)

{-# INLINABLE modularExponentiationFp2 #-}
modularExponentiationFp2 :: Fp2 -> BuiltinByteString -> Fp2
modularExponentiationFp2 b e
    | popCountByteString e == 0  = one
    | otherwise = t * modularExponentiationFp2 (b*b) (shiftByteString e (-1))
                where t = if testBitByteString e 0 then b else one

instance Module Integer Fp2 where
    {-# INLINABLE scale #-}
    scale :: Integer -> Fp2 -> Fp2
    scale a b = modularExponentiationFp2 b (reverseByteString (integerToByteString a))

instance MultiplicativeGroup Fp2 where
    {-# INLINABLE div #-}
    div a b | b == zero     = error ()
            | otherwise     = a * recip b
    {-# INLINABLE recip #-}
    recip (Fp2 a b) = Fp2 (a `div` norm) (negate b `div` norm)
        where norm = a*a + b*b

instance Ord Fp2 where
    {-# INLINABLE (<) #-}
    (<) :: Fp2 -> Fp2 -> Bool
    Fp2 a b < Fp2 c d = a < c || (a == c && b < d)
    {-# INLINABLE (<=) #-}
    Fp2 a b <= Fp2 c d = a <= c && b <= d
    {-# INLINABLE (>) #-}
    (>) :: Fp2 -> Fp2 -> Bool
    Fp2 a b > Fp2 c d = a > c || (a == c && b > d)
    -- {-# INLINABLE (>=) #-}
    -- Fp2 a b >= Fp2 c d =

{-# INLINABLE compressG2Point #-}
compressG2Point :: (Fp2,Fp2) -> BuiltinBLS12_381_G2_Element
compressG2Point (x, y)
    | x == zero && y == one = bls12_381_G2_zero
    | otherwise             = go (bls12_381_G2_uncompress xBsG2) y
    where x' = unFp (c0 x) + unFp (c1 x) * pow 2 384
          fixLengthBs :: BuiltinByteString -> BuiltinByteString
          fixLengthBs bs
                | lengthOfByteString bs == 96 = bs
                | otherwise                   = fixLengthBs (bs <> consByteString 0 emptyByteString)
          xBsG2 = reverseByteString $ writeBitByteString ((fixLengthBs . integerToByteString) x') 7 True
          go :: BuiltinBLS12_381_G2_Element -> Fp2 -> BuiltinBLS12_381_G2_Element
          go x y
            | y < negate y   = x
            | otherwise      = bls12_381_G2_neg x

-- This function is for testing only, as the sqrt over Fp used the expensive scale function
-- so it will cost around 20% of the script cpu budget.
{-# INLINABLE unCompressG2Point #-}
unCompressG2Point :: BuiltinBLS12_381_G2_Element -> (Fp2, Fp2)
unCompressG2Point p
    | p == bls12_381_G2_zero = (zero, one)
    | otherwise              = (x, y')
        where p' = reverseByteString . bls12_381_G2_compress $ p
              thirdBit = testBitByteString p' 5
              xBs = foldr (\i acc -> writeBitByteString acc i False) p' [7,6,5]
              x = Fp2 (Fp . byteStringToInteger $ sliceByteString 0 48 xBs) (Fp . byteStringToInteger $ sliceByteString 48 96 xBs)
              Fp2 c d = x * x * x + Fp2 (Fp 4) (Fp 4)
              sqrt :: Fp -> Fp
              sqrt = scale ((bls12_381_base_prime + 1) `divide` 4)
              a = sqrt ((c + sqrt (c * c + d * d)) `div` Fp 2)
              b' = sqrt ((negate c + sqrt (c * c + d * d)) `div` Fp 2)
              b = if a * b' * Fp 2 == d then b' else negate b'
              y = Fp2 a b
              y' = if thirdBit && y < negate y then negate y else y


instance AdditiveSemigroup BuiltinBLS12_381_G1_Element where
    {-# INLINABLE (+) #-}
    (+) = bls12_381_G1_add

instance AdditiveMonoid BuiltinBLS12_381_G1_Element where
    {-# INLINABLE zero #-}
    zero = bls12_381_G1_zero

instance AdditiveGroup BuiltinBLS12_381_G1_Element where
    {-# INLINABLE (-) #-}
    (-) a b = a + bls12_381_G1_neg b

instance Module Scalar BuiltinBLS12_381_G1_Element where
    {-# INLINABLE scale #-}
    scale (Scalar a) = bls12_381_G1_scalarMul a

instance AdditiveSemigroup BuiltinBLS12_381_G2_Element where
    {-# INLINABLE (+) #-}
    (+) = bls12_381_G2_add

instance AdditiveMonoid BuiltinBLS12_381_G2_Element where
    {-# INLINABLE zero #-}
    zero = bls12_381_G2_zero

instance AdditiveGroup BuiltinBLS12_381_G2_Element where
    {-# INLINABLE (-) #-}
    (-) a b = a + bls12_381_G2_neg b

instance Module Scalar BuiltinBLS12_381_G2_Element where
    {-# INLINABLE scale #-}
    scale (Scalar a) = bls12_381_G2_scalarMul a