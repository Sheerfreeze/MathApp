module Language.Math.Algebra.Types (
    Polarity,
    Coefficient,
    Degree,
    Base,
    Operation(..),
    Polynomial(..),
    Formula(..),
    Equation
) where

import Data.Maybe

type Polarity = Bool
type Coefficient = Double
type Degree = Double

data Operation = Add | Subtract | Multiply | Divide | Power deriving (Show, Eq)
data Polynomial = Head | LinkingOp Operation | Component Formula deriving (Show, Eq)

type Base = Maybe Char

data Formula = Val Double |
               Var Char |
               Group [Polynomial] deriving (Eq)

instance Show Formula where
    show (Val x) = show x
    show (Var x) = [x]
    show (Group []) = ")"
    show (Group (Head:xs)) = "(" ++ show (Group xs)
    show (Group ((Component part):xs)) = (show part) ++ show (Group xs) --Eventually only wrap ends and beginnings in ()
    show (Group ((LinkingOp lOp):xs)) = case lOp of
            Add -> "+" ++ show (Group xs)
            Subtract -> "-" ++ show (Group xs)
            Multiply -> "*" ++ show (Group xs)
            Divide -> "/" ++ show (Group xs)
            Power -> "^" ++ show (Group xs)


type Equation = (Formula, Ordering, Formula)
