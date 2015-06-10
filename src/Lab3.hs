module Lab3 where

import Control.Applicative
import Data.Char
import Data.Monoid
import Parser

type Var = String
type Op = String
type Rel = String

data Arith = Val Integer
           | Var Var
           | Binary Arith Op Arith
           deriving (Show, Eq)

data Exp = AExp Arith
         | RExp Arith Rel Arith
         deriving (Show, Eq)

op :: (b -> a -> b -> c) -> (a -> b -> b -> c)
op f a x1 x2 = f x1 a x2

identifier = trace "identifier" $ some (satisfy isAlpha anyChar)

expression = trace "expression" $ RExp <$> arithmetic <*> relation <*> arithmetic
             <|> AExp <$> arithmetic

arithmetic = trace "arithmetic" $ chainl1 term (op Binary <$> addition)

term = trace "term" $ chainl1 factor (op Binary <$> production)

factor = trace "factor" $ Var <$> identifier <|> Val <$> number <|> between (char '(') (char ')') arithmetic

relation = trace "relation" $ strOf [ "<", "<=", "=", "<>", ">", ">=" ]

addition = trace "addition" $ strOf [ "+", "-" ]

production = trace "production" $ strOf [ "*", "/" ]

parseE p = parse (p <* eof) . filter (not . isSpace)
