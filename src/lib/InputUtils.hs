module InputUtils 
( dos2unix 
, dropBom
, split
, ParseCsvOpts(..)
, parseCsv
, parseCsvString
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
    s <- readFile f 
    return (parseCsvString s opts)

-- | 'parseCsvString' takes a string and reads its contents as csv
parseCsvString :: String -> ParseCsvOpts -> [[String]]
parseCsvString s opts = [getRow line | line <- rows]
    where rows = if stripHeader opts then tail rows' else rows'
                 where rows' = lines . dos2unix . dropBom $ s
          getRow = case (stripNumbering opts, stripClassLabel opts) of
                      (True, True)   -> init . tail . split d 
                      (True, False)  -> tail . split d
                      (False, True)  -> init . split d
                      (False, False) -> split d
                   where d = delimiter opts

-- | Set of options for 'parseCsv'
data ParseCsvOpts = ParseCsvOpts
    { delimiter :: Char
    , stripHeader :: Bool
    , stripNumbering :: Bool
    , stripClassLabel :: Bool
    }