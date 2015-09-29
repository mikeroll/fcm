import InputUtils

main :: IO ()
main = do
    let opts = ParseCsvOpts { delimiter = ','
                            , stripHeader = False
                            , stripNumbering = False
                            , stripClassLabel = True
                            }
    parse_csv "input/butterfly.txt" opts >>= print

