import System.IO()

-- |'dos2unix' removes those filthy \r's
dos2unix :: String -> String
dos2unix = filter (/= '\r')

-- |'split' is like 'words', but splits on given delimiter, not just space
split     :: Char -> String -> [String]
split d s = case dropWhile (==d) s of 
                  "" -> []
                  s' -> w : split d s''
                        where (w, s'') = break (==d) s'

parse_csv   :: String -> IO [[String]]
parse_csv f = do
    contents <- readFile f
    return [split ',' line | line <- lines $ dos2unix contents]

main :: IO ()
main = do
    parse_csv "input/irises.txt" >>= print

