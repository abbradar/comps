{-# LANGUAGE ScopedTypeVariables #-}

module Lab6 where

import Control.Arrow
import Data.Maybe
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State
import Lab5a

data NodeType = F | G
              deriving (Show, Eq, Ord)

type Node a = (NodeType, a)

type PrecedenceTable a = (Map a Integer, Map a Integer)

precedenceTable :: forall a. Ord a => RelationTable a -> PrecedenceTable a
precedenceTable table = toPrec F &&& toPrec G $ atoms
  where toPrec :: NodeType -> [a] -> Map a Integer
        toPrec t = M.fromList . map (\n -> (n, findLength (t, n)))

        atoms = concatMap (\((a, b), _) -> [a, b]) $ M.toList table
        
        edges :: Map (Node a) [Node a]
        edges = M.fromList $ map (\vs@((n, _):_) -> (n, map snd vs)) $ groupBy ((==) `on` fst) $ sort $ mapMaybe edge $ M.toList table

        edge :: ((a, a), Ordering) -> Maybe (Node a, Node a)
        edge ((a, b), ord) = case ord of
          EQ -> Nothing
          LT -> Just ((G, b), (F, a))
          GT -> Just ((F, a), (G, b))

        findLength :: Node a -> Integer
        findLength n = case M.lookup n edges of
          Nothing -> 0
          Just ns -> 1 + maximum (map findLength ns)

precOp :: OpCompare
precOp op a b = (f M.! a) `compare` (g M.! b)
  where (f, g) = precedenceTable $ relationTable op

precOpParser :: [Op String] -> String -> Parsed String
precOpParser ops = opParser' precOp ops . words
