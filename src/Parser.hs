
module Parser where 

import Prelude hiding (exp)

import Exp
import Data.Char (isLetter)
import Control.Applicative ((<|>),many)
import qualified Text.Parsec as P


data Raw
    = PList [Raw]
    | PInt Int
    | PPlus
    | PSub
    | PTimes
    | PChar Char
    | PLet
    | PLam
    | PApp
    | PSelf
    | PIfZero
    deriving Show


toExp :: Raw -> Either String Exp
toExp (PList l) =
    case l of
        [PPlus, r1, r2] -> EPlus <$> toExp r1 <*> toExp r2
        [PSub, r1, r2]  -> ESub <$> toExp r1 <*> toExp r2
        [PTimes, r1, r2] -> ETimes <$> toExp r1 <*> toExp r2
        [PLet,PChar x,r1,r2] -> ELet x <$> toExp r1 <*> toExp r2
        [PLam,PChar x,r] -> ELam x <$> toExp r
        [PApp,r1,r2] -> EApp <$> toExp r1 <*> toExp r2
        [PIfZero,r1,r2,r3] -> EIfZero <$> toExp r1 <*> toExp r2 <*> toExp r3
        (PInt _:_) -> Left "integer is not primitive operator"
        (PChar _:_) -> Left "variable is not primitive operator"
        (PSelf:_) -> Left "self is not primitive operator"
        (PList _:_) -> Left "fist arguments must be primitive operator"
        [] -> Left "empty expression is unavailable"
        _ -> Left "unavailable arguments"
toExp (PInt n) = Right (EInt n)
toExp (PChar x) = Right (EVar x)
toExp PSelf = Right (EVar '*')
toExp _ = Left "primitive operator must be first argument"


int =  PInt . read <$> P.many1 P.digit
plus = const PPlus <$> P.string "+"
sub = const PSub <$> P.string "-"
times = const PTimes <$> P.string "*"
var = PChar <$> P.satisfy isLetter
let' = const PLet <$> P.try (P.string "let")
lam = const PLam <$> P.try (P.string "lam")
app = const PApp <$> P.try (P.string "app")
self = const PSelf <$> P.try (P.try (P.string "self"))
iz = const PIfZero <$> P.try (P.string "iz")

prim = int <|> plus <|> sub <|> times <|> let' <|> lam <|> app <|> self <|> iz <|> var

spaces = P.space <|> P.tab <|> P.newline
ss = P.skipMany spaces
ss1 = P.skipMany1 spaces

exp = list <|> prim

list = P.string "(" *> (PList <$> ((:) <$> exp <*> many (ss1 >> exp))) <* (ss >> P.string ")")

parse :: P.SourceName -> String -> Either P.ParseError Raw
parse = P.parse exp
