module Main where

import Data.List

import InputUtils
import FCM

import Options.Applicative
import Options

program :: Options -> IO ()
program (Options infile inputOpts classifierOpts) = do
    objects <- loadObjects infile inputOpts
    let memberships = clusterize classifierOpts objects
    let result = transpose memberships
    putStrLn $ unlines $ map show result

main :: IO ()
main = execParser fcm_opts >>= program
