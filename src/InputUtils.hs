module InputUtils 
( dos2unix 
, dropBom
, split
, parse_csv
) where

-- |'dos2unix' removes those filthy \r's
dos2unix :: String -> String
dos2unix = filter (/= '\r')

-- |'dropBom' drops a UTF-8 BOM if it's present
dropBom :: String -> String
dropBom ('\xfeff':s) = s
dropBom s = s

-- |'split' is like 'words', but splits on given delimiter, not just space
split     :: Char -> String -> [String]
split d s = case dropWhile (==d) s of 
                  "" -> []
                  s' -> w : split d s''
                        where (w, s'') = break (==d) s'

-- |'parse_csv' takes a file and reads its contents as csv
parse_csv   :: FilePath -> IO [[String]]
parse_csv f = do
    contents <- readFile f
    return [split ',' line | line <- lines . dos2unix . dropBom $ contents ]