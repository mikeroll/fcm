module FCM
( hammingDistance
, euclidDistance
) where

hammingDistance :: (Floating x) => [x] -> [x] -> x
hammingDistance x1 x2 = sum . map abs $ zipWith (-) x1 x2 

euclidDistance :: (Floating x) => [x] -> [x] -> x
euclidDistance x1 x2 = sqrt . sum $ zipWith (-) x1 x2