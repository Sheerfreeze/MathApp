module Main where

import Language.Math.Algebra
import Control.Monad
import Data.Monoid
import Data.Char

validateInput :: [String] -> Bool
validateInput inLst = length inLst == 1 && (and . concat . map (map isDigit)) inLst

main :: IO ()
main = do
    clearScreen
    putStrLn "\nThe Math App ~ Brooks & Duncan"
    putStrLn "\t1) Finding & Proving Inverses"
    putStrLn "\t2) Etc, etc..."
    putStrLn "\t3) Quit"
    uc <- words <$> getLine
    if(validateInput uc)
        then case (read . head $ uc) of
            1 -> do
                clearScreen
                inverses
                uc <- getLine
                main
            3 -> putStrLn "Farwell..."
            _ -> do
                clearScreen
                putStrLn "\nSorry, invalid choice, choose again."
                uc <- getLine
                main
        else do
            clearScreen
            putStrLn "Malformed input, please enter a single number choice."
            uc <- getLine
            main
