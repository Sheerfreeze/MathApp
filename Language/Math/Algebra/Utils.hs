module Language.Math.Algebra.Utils (
    clearScreen
) where

import Control.Monad

clearScreen :: IO ()
clearScreen = replicateM_ 100 (putStrLn "")
