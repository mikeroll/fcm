{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import Data.List

import InputUtils
import Math
import FCM

data FCMArgs = FCMArgs
    { infile  :: FilePath

    , stripHeader :: Bool
    , stripNumbering :: Bool
    , stripClassLabel :: Bool
    , delimiter :: String

    , clusters :: Int
    , fuzziness :: Double
    , threshold :: Double
    , distance :: Distance
    } deriving (Show, Data, Typeable)

fcm_args = FCMArgs
    { infile          = ""    &= help "Input file"

    , stripHeader     = False &= help "Strip first line"
    , delimiter       = ","   &= opt "delimiter" &= help "Input files delimiter"
    , stripNumbering  = False &= help "Strip first column (numbers)"
    , stripClassLabel = False &= help "Strip last column (class labels)"

    , clusters        = 2     &= help "Number of clusters"
    , fuzziness       = 2     &= help "Fuzziness parameter"
    , threshold       = 1e-4  &= help "Convergence threshold"
    , distance        = enum
                        [ Hamming &= help "Hamming distance (default)"
                        , Euclidean &= help "Euclidean distance"
                        ]
    }

main :: IO ()
main = do
    fcm_opts      <- cmdArgs fcm_args

    let input_opts = InputOpts {
      _stripHeader     = stripHeader fcm_opts
    , _delimiter       = (delimiter fcm_opts) !! 0
    , _stripNumbering  = stripNumbering fcm_opts
    , _stripClassLabel = stripClassLabel fcm_opts
    }

    let classifier_opts = ClassifierOpts {
      _clusters  = clusters fcm_opts
    , _fuzziness = fuzziness fcm_opts
    , _threshold = threshold fcm_opts
    , _distance  = distance fcm_opts
    }

    let src = infile fcm_opts

    objects <- loadObjects src input_opts
    let memberships = clusterize classifier_opts objects
    let result = transpose memberships
    putStrLn $ unlines $ map show result
