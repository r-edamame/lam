
module VM (run) where

import Exp (Exp(..))

import Text.Printf (printf)

type Env = [(String,Value)]

data Value
    = Const Int
    | Closure String [Prog] Env

instance Show Value where
    show (Const n) = show n
    show (Closure _ _ _) = "#closure"

data Prog
    = RInt Int
    | RPlus
    | RSub
    | RTimes
    | RVar String
    | RLetin String
    | RPop
    | RLam String [Prog]
    | RApp
    | RRestore [Prog] Env Cont
    | RIfZero [Prog] [Prog]


instance Show Prog where
    show (RInt n) = printf "RInt %d" n
    show RPlus = "RPlus"
    show RSub = "RSub"
    show RTimes = "RTimes"
    show (RVar x) = printf "RVar %c" x
    show (RLetin x) = printf "RLetin %c" x
    show RPop = "RPop"
    show (RLam x p) = printf "Lam"
    show RApp = "RApp"
    show (RRestore p e c) = "RRestore"
    show (RIfZero et ef) = "RIfZero"

type Cont = [Value]

data Machine
    = Eval [Prog] Env Cont
    | Apply Cont Value
    | Finish Value
    | Error String
    deriving Show

trans :: Machine -> Machine
trans (Eval [] _ [v]) = Finish v
trans (Eval [] _ _)   = Error "program stopped, but continuation still remains"
trans (Eval (r:rs) env cont) =
    case (r,cont) of
        (RInt n,_)  -> Eval rs env (Const n:cont)
        (RPlus,Const x:Const y:cont') -> Eval rs env (Const (x+y):cont')
        (RSub,Const x:Const y:cont') -> Eval rs env (Const (y-x):cont')
        (RTimes,Const x:Const y:cont') -> Eval rs env (Const (x*y):cont')
        (RVar x,_) ->
            maybe
                (Error $ printf "variable %c not found" x)
                (\v -> Eval rs env (v:cont))
                (lookup x env)
        (RLetin x,v:cont') -> Eval rs ((x,v):env) cont'
        (RPop,_) -> Eval rs (tail env) cont
        (RLam x p',_) -> Eval rs env (Closure x p' env:cont)
        (RApp,v:Closure x p env':cont') -> Eval (concat [p,[RRestore rs env cont']]) ((x,v):("*",Closure x p env'):env') []
        (RRestore p env' cont',v:_) -> Eval p env' (v:cont')
        (RIfZero pt pf,Const n:cont')
            | n == 0    -> Eval (pt++rs) env cont'
            | otherwise -> Eval (pf++rs) env cont'
        _ -> Error $ "some exception occurred"


compile :: Exp -> [Prog]
compile (EInt n) = [RInt n]
compile (EPlus e1 e2) = concat [compile e1, compile e2, [RPlus]]
compile (ESub e1 e2) = concat [compile e1, compile e2, [RSub]]
compile (ETimes e1 e2) = concat [compile e1, compile e2, [RTimes]]
compile (EVar x) = [RVar x]
compile (ELet x e1 e2) = concat [compile e1, [RLetin x], compile e2, [RPop]]
compile (ELam x e) = [RLam x (compile e)]
compile (EApp e1 e2) = concat [compile e1, compile e2, [RApp]]
compile (EIfZero eb et ef) = compile eb ++ [RIfZero (compile et) (compile ef)]


run :: Exp -> Env -> Either String Value
run exp env = go (Eval (compile exp) env []) where
    go (Finish n) = Right n
    go (Error m)  = Left m
    go m = go (trans m)
