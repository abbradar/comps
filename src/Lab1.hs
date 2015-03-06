{-# LANGUAGE LambdaCase, ViewPatterns #-}

import Control.Arrow
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Parser
import Rules

type RightProduct = (Expression, Maybe Nonterm)
type RightExpression = [RightProduct]
type RightRule = (Nonterm, RightExpression)
type RightRules = Map Nonterm RightExpression

rightProduct :: Expression -> RightProduct
rightProduct Empty = (Empty, Nothing)
rightProduct (NT nt) = (Empty, Just nt)
rightProduct (Product t@(T _) b) = let (e, nt) = rightProduct b
                                 in (Product t e, nt)
rightProduct _ = error "rightProduct: not a right-aligned grammar"

rightAligned :: Expression -> RightExpression
rightAligned (Sum a b) = rightProduct a : rightAligned b
rightAligned a = [rightProduct a]

solve :: RightRules -> Rules
solve = transform . backward . forward . Map.toList
  where replace :: RightRule -> RightExpression -> RightExpression
        replace (nonterm, expr) = concatMap $ \case
          (reg, Just nonterm') | nonterm == nonterm' -> map (first $ Product reg) expr
          p -> [p]

        productStar :: [Expression] -> Expression
        productStar [] = Empty
        productStar (h:t) = Product (productStar $ map (Product $ Star h) t) (Star h)

        pass :: (RightRule -> RightExpression) -> [RightRule] -> [RightRule]
        pass _ [] = []
        pass f (r@(nonterm, _):t) = rule:forward (map (second $ replace rule) t)
          where rule = (nonterm, f r)

        forward :: [RightRule] -> [RightRule]
        forward = pass replaceCycle
          where replaceCycle (nonterm, expr) = map (first $ Product prod) expr'
                  where (self, expr') = partition (maybe False (== nonterm) . snd) expr
                        prod = productStar $ map fst self

        backward :: [RightRule] -> [RightRule]
        backward = pass snd . reverse

        transform :: [RightRule] -> Rules
        transform = Map.fromList . map (second $ foldr1 Sum . map (\(x, Nothing) -> x))

main :: IO ()
main = do
  rs <- readFile "rules"
  Grammar axiom rules <- case parse grammar rs of
   Right r -> return r
   Left e -> fail $ show e
  let regexp = simplify $ (Map.! axiom) $ solve $ Map.map rightAligned rules
  putStrLn $ toString regexp
  input <- getLine
  putStrLn $ show $ parse (parseExpression regexp) input
