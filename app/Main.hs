module Main where

import           Lib          ( Args(..), parseArgs, readInput, processInput
                              , writeOutput)


usage = "Bad parameters.\n\nPlease provide 2 valid filenames, the first one pointing to \
        \an input file to read the data from, and the second one pointing where \
        \to write the results to. The filenames must not be the same.\n\n\
        \(You can use - as the output filename to print the results to stdout \
        \(because you don't want to name files '-' anyway :-) )"


main :: IO ()
main = do
    args <- parseArgs
    case args of
        Nothing -> putStrLn usage

        Just (Args inFn outFn) -> do
            x <- readInput inFn 
            let y = processInput x
            writeOutput outFn y
