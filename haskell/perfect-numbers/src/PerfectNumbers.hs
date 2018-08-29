module PerfectNumbers (classify, Classification(..)) where

import Numeric.Natural
import Data.List

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing
  | otherwise = Just $ classifyNatural (toEnum n)

classifyNatural :: Natural -> Classification
classifyNatural n
  | aliquotSum n == n = Perfect
  | aliquotSum n >  n = Abundant
  | aliquotSum n <  n = Deficient

aliquotSum :: Natural -> Natural
aliquotSum n = sum $ filter ((/=) n) (factors n)

factors :: Natural -> [Natural]
factors 0 = []
factors x = nub $ product <$> (subsequences $ primeFactors x)

primeFactors :: Natural -> [Natural]
primeFactors 0 = []
primeFactors x = go x 2 [1]
 where
  go n c fs
    | n == 1 = fs
    | n `mod` c == 0 = go (n `div` c) c (c:fs)
    | otherwise = go n (c + 1) fs
