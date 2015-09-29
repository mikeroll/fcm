-- |'split' is like 'words', but splits on given delimiter, not just space
split     :: Char -> String -> [String]
split d s = case dropWhile (==d) s of 
                  "" -> []
                  s' -> w : split d s''
                        where (w, s'') = break (==d) s'


main = print $ split ',' "a,b,c,d"

