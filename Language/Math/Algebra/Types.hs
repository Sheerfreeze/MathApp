module Language.Math.Algebra.Types (
    Polarity,
    Coefficient,
    Degree,
    Base(..),
    Operation(..),
    LinkingOp,
    Formula(..),
    Equation
) where

type Polarity = Bool
type Coefficient = Double
type Degree = Double

data Operation = Add | Subtract | Multiply | Divide deriving (Show, Eq)
type LinkingOp = Operation

data Base = Var Char | Val Double deriving (Show, Eq)

data Formula = Mono {polarity :: Polarity,
                     coefficient :: Coefficient,
                     base :: Base,
                     degree :: Degree} |
               Group [(LinkingOp, Formula)] deriving (Show, Eq)

type Equation = (Formula, Ordering, Formula)
