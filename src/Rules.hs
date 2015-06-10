{-# LANGUAGE ViewPatterns #-}

module Rules
       ( Expression(..)
       , toString
       , Nonterm
       , Rules
       , Grammar(..)
       , grammar
       , simplify
       , simplifyAll
       , parseExpression
       ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Parser

type Nonterm = Char

data Expression = NT Nonterm
                | T Char
                | Empty
                | Product Expression Expression
                | Sum Expression Expression
                | Star Expression
                deriving (Show, Eq)

braced :: (Expression -> Bool) -> Expression -> String
braced _ e@(NT _) = toString e
braced _ e@(T _) = toString e
braced _ e@Empty = toString e
braced f x
  | f x = toString x
  | otherwise = "(" ++ toString x ++ ")"

toString :: Expression -> String
toString (NT x) = pure x
toString (T x) = pure x
toString Empty = "#"
toString (Product a b) = braced isP a ++ braced isP b
  where isP (Product _ _) = True
        isP (Star _) = True
        isP _ = False
toString (Sum a b) = toString a ++ " + " ++ toString b
toString (Star a) = braced isS a ++ "*"
  where isS (Star _) = True
        isS _ = False

type Rules = Map Nonterm Expression
data Grammar = Grammar Nonterm Rules
             deriving (Show, Eq)

nonterm :: Parser Nonterm
nonterm = upper

letter :: Parser Expression
letter = (char '#' >> pure Empty) <|> (NT <$> nonterm) <|> (T <$> satisfy (\x -> isLower x || isNumber x) anyChar)

expression :: Parser Expression
expression = foldr1 Sum <$> (foldr1 Product <$> letter `sepBy` spaces) `sepBy` (spaces >> char '+' >> spaces)

rule :: Parser (Nonterm, Expression)
rule = (,) <$> nonterm <*> (spaces >> string "->" >> spaces >> expression)

checkGrammar :: Rules -> Expression -> Bool
checkGrammar r (NT nt) = nt `Map.member` r
checkGrammar _ (T _) = True
checkGrammar _ Empty = True
checkGrammar r (Sum a b) = checkGrammar r a && checkGrammar r b
checkGrammar r (Product a b) = checkGrammar r a && checkGrammar r b
checkGrammar r (Star a) = checkGrammar r a

grammar :: Parser Grammar
grammar = do
  axiom <- nonterm
  void newline
  rules <- Map.fromListWith Sum <$> rule `sepBy` newline
  when (axiom `Map.notMember` rules) $ fail "grammar: axiom should be defined in rules"
  unless (Map.fold (&&) True $ Map.map (checkGrammar rules) rules) $ fail "grammar: unspecified nonterminals"
  return $ Grammar axiom rules

removeEmpty :: (Expression -> Expression -> Expression) -> Expression -> Expression -> Expression
removeEmpty _ Empty b = b
removeEmpty _ a Empty = a
removeEmpty f a b = f a b

simplify :: Expression -> Expression
simplify (Product a b) = removeEmpty Product (simplify a) (simplify b)
simplify (Sum a b)
  | a == b = simplify a
  | otherwise = Sum (simplify a) (simplify b)
simplify (Star (simplify -> a)) = case a of
  Empty -> a
  Star _ -> a
  Sum e1 e2 -> Star $ removeEmpty Sum e1 e2
  _ -> Star a
simplify a = a

simplifyAll :: Rules -> Rules
simplifyAll rules = if Map.null lambdas
                    then good
                    else simplifyAll $ Map.foldWithKey (\nt _ -> Map.map (deleteLambda nt) . Map.delete nt) rules lambdas
  where (lambdas, good) = Map.partition isE $ Map.map simplify rules

        isE Empty = True
        isE _ = False

        deleteLambda nt n@(NT nt')
          | nt == nt' = Empty
          | otherwise = n
        deleteLambda nt (Sum a b) = Sum (deleteLambda nt a) (deleteLambda nt b)
        deleteLambda nt (Product a b) = Product (deleteLambda nt a) (deleteLambda nt b)
        deleteLambda nt (Star a) = Star $ deleteLambda nt a
        deleteLambda _ a = a

parseExpression :: Expression -> Parser String
parseExpression e = expr e <* eof
  where expr (T c) = pure <$> char c
        expr (NT _) = fail "exp: non-terminals are not allowed"
        expr Empty = pure ""
        expr (Product a b) = (++) <$> expr a <*> expr b
        expr (Sum a b) = expr a <|> expr b
        expr (Star a) = concat <$> many (expr a)
