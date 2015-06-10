{-# LANGUAGE ViewPatterns #-}

import Control.Applicative
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Parser
import Lab3

data Operator = Operator Var Exp
              deriving (Show, Eq)

program = trace "program" $ block

block = trace "block" $ string "begin" *> operators <* string "end"

operators = trace "operators" $ some $ operator <* char ';'

operator = trace "operator" $ Operator <$> identifier <* char '=' <*> expression

type State = Map Var Integer

evalA :: State -> Arith -> Integer
evalA _ (Val i) = i
evalA st (Var n) = st M.! n
evalA st (Binary (evalA st -> r1) op (evalA st -> r2)) = case op of
  "+" -> r1 + r2
  "-" -> r1 - r2
  "*" -> r1 * r2
  "/" -> r1 `div` r2

eval :: State -> Exp -> Integer
eval st (AExp ar) = evalA st ar
eval st (RExp (evalA st -> r1) op (evalA st -> r2))
  | r = 1
  | otherwise = 0

  where r = case op of
          "<" -> r1 < r2
          "<=" -> r1 <= r2
          "=" -> r1 == r2
          "<>" -> r1 /= r2
          ">" -> r1 > r2
          ">=" -> r1 >= r2

apply :: State -> Operator -> State
apply st (Operator var e) = M.insert var (eval st e) st

exec :: [Operator] -> State
exec = foldl apply M.empty
