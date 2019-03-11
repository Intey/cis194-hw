import Log
import LogAnalysis


main = do 
    filepath <- getLine
    messages <- parse <$> readFile filepath
    putStr $ foldr (\acc x -> acc ++ "\n" ++ x) "" $ take 100 . map show $ inOrder $ build messages
