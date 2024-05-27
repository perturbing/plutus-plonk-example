{-# LANGUAGE NumericUnderscores #-}

module PlutusBenchmark.ProtocolParameters (maxTxSize, maxTxExSteps, maxTxExMem)
where

-- Protocol parameters (June 2023)

{- | This is the "maximum transaction size".  We're just comparing the size of
the script with this, so our results may be a little optimistic if the
transaction includes other stuff (I'm not sure exactly what "maximum
transaction size" means).
-}
maxTxSize :: Integer
maxTxSize = 16_384

maxTxExSteps :: Integer
maxTxExSteps = 10_000_000_000

maxTxExMem :: Integer
maxTxExMem = 14_000_000
