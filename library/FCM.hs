module FCM
( initMemberships
, nextCenters
, nextMemberships
, converge
, clusterize
) where

import Data.List
import System.Random

import Math

-- convenience aliases
type Vector = [Double]
type Matrix = [[Double]]

-- | chunks a list into a matrix
chunks :: (Floating f) => Int -> [f] -> [[f]]
chunks n = takeWhile (not.null) . unfoldr (Just . splitAt n)

-- | sets up random initial memberships
initMemberships :: Int -> Int -> Matrix
initMemberships clusters_n objects_n =
    transpose . map normalize $ chunks clusters_n $
        take n $ randomRs randomRange $ mkStdGen randomSeed
      where
        n = clusters_n * objects_n
        normalize m = m `divV` (sum m)
        randomSeed = 256
        randomRange = (1, fromIntegral randomSeed)

-- | calculates new cluster centers
nextCenters :: Double -> Matrix -> [Vector] -> [Vector]
nextCenters m memberships objects = map nextCenter memberships
  where
    nextCenter cluster = weighted_xs `divV` weights
      where
        weighted_xs = sumV $ zipWith (\u x -> x `mulV` (u**m)) cluster objects
        weights = sum $ map (**m) cluster

-- | reflows memberships
nextMemberships :: DistanceFunc -> Double -> [Vector] -> [Vector] -> Matrix
nextMemberships df m objects centers = transpose $ map nextMembership objects
  where
    nextMembership object = map nextCoef centers
      where
        nextCoef center = (1 /) . sum $ map term centers
          where
            term center' = (df object center / df object center') ** (2 / (m - 1))

-- | converges memberships matrix to within given error
converge :: DistanceFunc -> Double -> Double -> Matrix -> [Vector] -> Matrix
converge df m e memberships objects
    | absMaximumM (memberships' `subM` memberships) < e = memberships'
    | otherwise = converge df m e memberships' objects
    where
      centers' = nextCenters m memberships objects
      memberships' = nextMemberships df m objects centers'

-- | main algorithm entry point
clusterize :: DistanceFunc -> Int -> Double -> Double -> [Vector] -> Matrix
clusterize df clusters_n m e objects =
    converge df m e initial objects
      where
        initial = initMemberships clusters_n $ length objects

