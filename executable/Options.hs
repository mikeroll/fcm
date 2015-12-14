module Options
( Options(..)
, fcm_opts
) where

import Options.Applicative
import Options.Applicative.Types

import InputUtils
import FCM
import Math



data Options = Options
    { infile         :: FilePath

    , inputOpts      :: InputOpts
    , classifierOpts :: ClassifierOpts
    }

opts :: Parser Options
opts = Options
    <$> argument str
        ( metavar "INFILE" )
    <*> inputOpts
    <*> classifierOpts
  where
    inputOpts = InputOpts
        <$> option str
            ( long "delimiter"
           <> short 'd'
           <> value ","
           <> showDefault
           <> help "Input files delimiter" )
        <*> switch
            ( long "strip-header"
           <> help "Strip first line" )
        <*> switch
            ( long "strip-numbering"
           <> help "Strip first column (numbers)" )
        <*> switch
            ( long "strip-class-label"
           <> help "Strip last column (class labels)" )
    classifierOpts = ClassifierOpts
        <$> option auto
            ( long "clusters"
           <> short 'c'
           <> value 3
           <> showDefault
           <> help "Number of clusters" )
        <*> option auto
            ( long "fuzziness"
           <> short 'f'
           <> value 2.0
           <> showDefault
           <> help "Fuzziness parameter" )
        <*> option auto
            ( long "threshold"
           <> short 'e'
           <> value 1e-4
           <> showDefault
           <> help "Convergence threshold" )
        <*> option auto
            ( long "distance"
           <> value Hamming
           <> showDefault
           <> help "Distance function to use, Hamming | Euclidean" )

fcm_opts = info (helper <*> opts)
           ( fullDesc
          <> progDesc "Classify objects from INFILE using FCM method"
          <> header "FCM clustering sample program" )
