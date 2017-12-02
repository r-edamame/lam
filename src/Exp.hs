
module Exp where

data Exp
    = EInt Int
    | EPlus Exp Exp
    | ESub Exp Exp
    | ETimes Exp Exp
    | EVar String
    | ELet String Exp Exp
    | ELam String Exp
    | EApp Exp Exp
    | EIfZero Exp Exp Exp
    deriving Show
