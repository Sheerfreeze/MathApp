module Language.Math.Algebra.Utils (
    merge,
    substitution,
    clearScreen
) where

import Control.Monad
import Language.Math.Algebra.Types

merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

substitution :: Formula -> Char -> Formula -> Formula
substitution (Var i) v r = if (i == v)
    then r
    else Var i
substitution (Val i) _ _ = Val i
substitution (Group lst) v r  = Group $ Head : merge (map (\ (Component x) -> Component $ substitution x v r) comps) ops
    where comps = [Component x | Component x <- lst]
          ops = [LinkingOp x | LinkingOp x <- lst]

clearScreen :: IO ()
clearScreen = replicateM_ 100 (putStrLn "")
