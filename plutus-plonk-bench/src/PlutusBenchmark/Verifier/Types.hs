{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

module PlutusBenchmark.Verifier.Types 
( ProofJSONSnarkjs(..)
, PreInputsJSONSnarkjs(..)
)where

import Data.Aeson.TH
    ( defaultOptions, Options(fieldLabelModifier), deriveFromJSON )
import GHC.Generics ( Generic )


data ProofJSONSnarkjs = ProofJSONSnarkjs 
  { a        :: [String]
  , b        :: [String]
  , c        :: [String]
  , z        :: [String]
  , t1       :: [String]
  , t2       :: [String]
  , t3       :: [String]
  , wxi      :: [String]
  , wxiw     :: [String]
  , eval_a   :: String
  , eval_b   :: String
  , eval_c   :: String
  , eval_s1  :: String
  , eval_s2  :: String
  , eval_zw  :: String
  , protocol :: String
  , curve    :: String
  } deriving (Show, Generic)

$(deriveFromJSON defaultOptions {
    fieldLabelModifier = let f "a" = "A"
                             f "b" = "B"
                             f "c" = "C"
                             f "z" = "Z"
                             f "t1" = "T1"
                             f "t2" = "T2"
                             f "t3" = "T3"
                             f "wxi" = "Wxi"
                             f "wxiw" = "Wxiw"
                             f other = other
                         in f
} ''ProofJSONSnarkjs)

data PreInputsJSONSnarkjs = PreInputsJSONSnarkjs
    { protocol' :: String
    , curve'    :: String
    , nPublic'  :: Integer
    , power'    :: Integer
    , k_1'       :: String
    , k_2'       :: String
    , qm        :: [String]
    , ql        :: [String]
    , qr        :: [String]
    , qo        :: [String]
    , qc        :: [String]
    , s1        :: [String]
    , s2        :: [String]
    , s3        :: [String]
    , x_2'      :: [[String]]
    , w         :: String
    } deriving (Show, Generic)

$(deriveFromJSON defaultOptions {
    fieldLabelModifier = let f "protocol'" = "protocol"
                             f "curve'" = "curve"
                             f "nPublic'" = "nPublic"
                             f "power'" = "power"
                             f "k_1'" = "k1"
                             f "k_2'" = "k2"
                             f "qm" = "Qm"
                             f "ql" = "Ql"
                             f "qr" = "Qr"
                             f "qo" = "Qo"
                             f "qc" = "Qc"
                             f "s1" = "S1"
                             f "s2" = "S2"
                             f "s3" = "S3"
                             f "x_2'" = "X_2"
                             f other = other
                         in f
} ''PreInputsJSONSnarkjs)