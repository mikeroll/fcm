module FCM
( hammingDistance
, euclidDistance
) where

import Data.List

-- reinventing vectors
mulV :: (Floating e) => [e] -> e -> [e]
mulV v x = map (* x) v

divV :: (Floating e) => [e] -> e -> [e]
divV v x = map (/ x) v

sumV :: (Floating e) => [[e]] -> [e]
sumV = foldl1' (zipWith (+))

-- | computes Hamming distance between two vectors
hammingDistance :: (Floating x) => [x] -> [x] -> x
hammingDistance x1 x2 = sum . map abs $ zipWith (-) x1 x2

-- | computes Euclid distance between two vectors
euclidDistance :: (Floating x) => [x] -> [x] -> x
euclidDistance x1 x2 = sqrt . sum $ zipWith (-) x1 x2

-- | sets up initial memberships (for now they are just 1/(number of clusters))
initialMemberships :: (Floating f) => Int -> Int -> [[f]]
initialMemberships clusters_n objects_n =
    replicate clusters_n $
        replicate objects_n $ 1.0 / (fromIntegral clusters_n)

-- | calculates new cluster centers
nextCenters :: (Floating f) => [[f]] -> [[f]] -> [[f]]
nextCenters memberships objects =
    map nextCenter memberships
  where
    nextCenter cluster =
        weighted_xs `divV` weights
      where
        weighted_xs = sumV $ zipWith (\u x -> x `mulV` (u^1)) cluster objects
        weights = sum $ map (^1) cluster

-- | reflows memberships
-- nextMemberships ::
