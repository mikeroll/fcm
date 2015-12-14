module InputUtils
( dos2unix
, dropBom
, split
, InputOpts(..)
, parseCsvString
, loadObjects
) where

-- | Set of options for 'parseCsv'
data InputOpts = InputOpts
    { _delimiter :: String
    , _stripHeader :: Bool
    , _stripNumbering :: Bool
    , _stripClassLabel :: Bool
    }

-- | 'dos2unix' removes those filthy \r's
dos2unix :: String -> String
dos2unix = filter (/= '\r')

-- | 'dropBom' drops a UTF-8 BOM if it's present
dropBom :: String -> String
dropBom ('\xfeff':s) = s
dropBom s = s

-- TODO: support multichar delimiters
-- | 'split' is like 'words', but splits on given delimiter, not just space
split :: String -> String -> [String]
split ds@(d:_) s = case dropWhile (==d) s of
    "" -> []
    s' -> w : split ds s''
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
