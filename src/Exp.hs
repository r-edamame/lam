
module Exp where

data Exp
    = EInt Int
    | EPlus Exp Exp
    | ESub Exp Exp
    | ETimes Exp Exp
    | EVar Char
    | ELet Char Exp Exp
    | ELam Char Exp
    | EApp Exp Exp
    | EIfZero Exp Exp Exp
    deriving Show
