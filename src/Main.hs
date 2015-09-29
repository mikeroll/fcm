import InputUtils

main :: IO ()
main = do
    parse_csv "input/butterfly.txt" >>= print

