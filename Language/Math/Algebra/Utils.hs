module Language.Math.Algebra.Utils (
    merge,
    mkGroup,
    substitution,
    groupByOp,
    clearScreen
) where

import Control.Monad
import Language.Math.Algebra.Types

merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

mkGroup :: [Formula] -> [Operation] -> Formula
mkGroup flst oplst = Group $ Head : compsAndOps flst oplst
    where compsAndOps (x:xs) (z:zs) = Component x : LinkingOp z : compsAndOps xs zs
          compsAndOps lst [] = map Component lst
          compsAndOps [] _ = []

substitution :: Formula -> Char -> Formula -> Formula
substitution (Var i) v r = if (i == v)
    then r
    else Var i
substitution (Val i) _ _ = Val i
substitution (Group lst) v r  = Group $ Head : merge (map (\ (Component x) -> Component $ substitution x v r) comps) ops
    where comps = [Component x | Component x <- lst]
          ops = [LinkingOp x | LinkingOp x <- lst]

ungroup :: Formula -> [Polynomial]
ungroup (Group lst) = lst

groupByOp :: Formula -> Operation -> Formula
groupByOp (Group (Head:rest)) operation =
    Group $ Head : (ungroup $ groupByOp (Group rest) operation)
groupByOp (Group (Component x:LinkingOp op:Component y:rest)) operation =
    if (op == operation)
        then groupByOp (Group $ Component (mkGroup [x,y] [op]) : rest) operation
        else Group $ Component x : LinkingOp op: (ungroup $ groupByOp (Group $ Component y : rest) operation)
groupByOp (Group (x:xs)) operation = Group $ x : ungroup (groupByOp (Group xs) operation)
groupByOp (Group []) _ = Group []

lcd :: Formula -> Formula
lcd = undefined

clearScreen :: IO ()
clearScreen = replicateM_ 100 (putStrLn "")
