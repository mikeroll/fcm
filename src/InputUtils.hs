module InputUtils 
( dos2unix 
, dropBom
, split
, ParseCsvOpts(..)
, parseCsv
) where

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

-- | 'parseCsv' takes a file and reads its contents as csv
parseCsv :: FilePath -> ParseCsvOpts -> IO [[String]]
parseCsv f opts = do
    contents <- readFile f
    let rows' = lines . dos2unix . dropBom $ contents
        rows = if stripHeader opts then tail rows' else rows'
        d = delimiter opts
        getRow = case (stripNumbering opts, stripClassLabel opts) of
            (True, True)   -> init . tail . split d 
            (True, False)  -> tail . split d
            (False, True)  -> init . split d
            (False, False) -> split d
    return [getRow line | line <- rows]

-- | Set of options for 'parseCsv'
data ParseCsvOpts = ParseCsvOpts
    { delimiter :: Char
    , stripHeader :: Bool
    , stripNumbering :: Bool
    , stripClassLabel :: Bool
    }