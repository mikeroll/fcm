module FCM
( initMemberships
, nextCenters
, nextMemberships
, converges
, clusterize
) where

import Data.List
import System.Random

import Math

-- | chunks a list into a matrix
chunks :: (Floating f) => Int -> [f] -> [[f]]
chunks n = takeWhile (not.null) . unfoldr (Just . splitAt n)

-- | sets up random initial memberships
initMemberships :: Int -> Int -> [[Double]]
initMemberships clusters_n objects_n =
    transpose . map normalize $ chunks clusters_n $
        take n $ randomRs randomRange $ mkStdGen randomSeed
      where
        n = clusters_n * objects_n
        normalize m = m `divV` (sum m)
        randomSeed = 256
        randomRange = (1, fromIntegral randomSeed)

-- | calculates new cluster centers
nextCenters :: (Floating f) => f -> [[f]] -> [[f]] -> [[f]]
nextCenters m memberships objects = map nextCenter memberships
  where
    nextCenter cluster = weighted_xs `divV` weights
      where
        weighted_xs = sumV $ zipWith (\u x -> x `mulV` (u**m)) cluster objects
        weights = sum $ map (**m) cluster

-- | reflows memberships
nextMemberships :: (Floating f) => ([f] -> [f] -> f) -> f -> [[f]] -> [[f]] -> [[f]]
nextMemberships df m objects centers = transpose $ map nextMembership objects
  where
    nextMembership object = map nextCoef centers
      where
        nextCoef center = (1 /) . sum $ map term centers
          where
            term center' = (df object center / df object center') ** (2 / (m - 1))

-- | returns whether the iteration converges with error e
converges :: (Ord f, Floating f) => [[f]] -> [[f]] -> f -> Bool
converges a b e = absMaximumM (a `subM` b) < e

-- | main algorithm entry point
clusterize :: (Ord f, Floating f) => ([f] -> [f] -> f) -> f -> f -> [[f]] -> [[f]] -> [[f]]
clusterize df m e memberships objects
    | converges memberships memberships' e = memberships'
    | otherwise = clusterize df m e memberships' objects
    where
      centers' = nextCenters m memberships objects
      memberships' = nextMemberships df m objects centers'

