{-# LANGUAGE DeriveDataTypeable #-}

module InputUtils
( dos2unix
, dropBom
, split
, InputOpts(..)
, parseCsvString
, loadObjects
) where

import Data.Data
import Data.Typeable

-- | Set of options for 'parseCsv'
data InputOpts = InputOpts
    { _delimiter :: Char
    , _stripHeader :: Bool
    , _stripNumbering :: Bool
    , _stripClassLabel :: Bool
    } deriving (Show, Data, Typeable)

-- | 'dos2unix' removes those filthy \r's
dos2unix :: String -> String
dos2unix = filter (/= '\r')

-- | 'dropBom' drops a UTF-8 BOM if it's present
dropBom :: String -> String
dropBom ('\xfeff':s) = s
dropBom s = s

-- | 'split' is like 'words', but splits on given delimiter, not just space
split :: Char -> String -> [String]
split d s = case dropWhile (==d) s of
    "" -> []
    s' -> w : split d s''
          where (w, s'') = break (==d) s'

-- | 'parseCsvString' takes a string and reads its contents as csv
parseCsvString :: String -> InputOpts -> [[String]]
parseCsvString s opts = [getRow line | line <- rows]
    where rows = if _stripHeader opts then tail rows' else rows'
                 where rows' = lines . dos2unix . dropBom $ s
          getRow = case (_stripNumbering opts, _stripClassLabel opts) of
                      (True, True)   -> init . tail . split d
                      (True, False)  -> tail . split d
                      (False, True)  -> init . split d
                      (False, False) -> split d
                   where d = _delimiter opts

-- | Load features from file
loadObjects :: FilePath -> InputOpts -> IO [[Double]]
loadObjects f opts = do
    s <- readFile f
    let raw = parseCsvString s opts
    let toVector = map (read :: String -> Double)
    let objects = map toVector raw
    return objects
