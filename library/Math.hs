module Math
( mulV
, divV
, sumV
, subM
, absMaximumM
, DistanceFunc
, hammingDistance
, euclidDistance
) where

import Data.List

-- | multilies a vector by a number
mulV :: (Num e) => [e] -> e -> [e]
mulV v x = map (* x) v

-- | divides a vector by a number
divV :: (Floating e) => [e] -> e -> [e]
divV v x = map (/ x) v

-- | subtracts two vectors
subV :: (Num e) => [e] -> [e] -> [e]
subV = zipWith (-)

-- | sums a list of vectors
sumV :: (Num e) => [[e]] -> [e]
sumV = foldl1' (zipWith (+))

-- | subtracts two matrices
subM :: (Num e) => [[e]] -> [[e]] -> [[e]]
subM = zipWith subV

-- | max absolute value in matrix
absMaximumM :: (Num e, Ord e) => [[e]] -> e
absMaximumM = maximum . map (\v -> maximum $ map abs v)

type DistanceFunc = [Double] -> [Double] -> Double

-- | computes Hamming distance between two vectors
hammingDistance :: DistanceFunc
hammingDistance x1 x2 = sum . map abs $ x1 `subV` x2

-- | computes Euclid distance between two vectors
euclidDistance :: DistanceFunc
euclidDistance x1 x2 = sqrt . sum $ zipWith (\a b -> (a - b)**2) x1 x2
