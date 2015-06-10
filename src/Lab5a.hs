{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

module Lab5a where

import Data.Maybe
import Data.List
import Control.Arrow
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State
import Debug.Trace

data Fixity = Prefix | Infixl | Infixr
            deriving (Show, Eq)

data Op a = Op { term :: a
               , pri :: Integer
               , fixty :: Fixity
               }
          deriving (Show, Eq)

data Atom a = RT a
            | RS
            | RA
            | ROBrace
            | RCBrace
            deriving (Show, Eq, Ord)

type RelationTable a = Map (a, a) Ordering

relationTable :: forall a. (Eq a, Ord a) => [Op a] -> RelationTable (Atom a)
relationTable ops =
  let ords = concatMap (\op -> map (first $ \a -> (RT $ term op, RT a)) $ rel op) ops
      generic = concatMap genrel ops
      extra = [ ((ROBrace, RCBrace), EQ)
              , ((RS, ROBrace), LT)
              , ((RS, RA), LT)
              , ((ROBrace, ROBrace), LT)
              , ((RA, RS), GT)
              , ((RCBrace, RS), GT)
              , ((ROBrace, RA), LT)
              , ((RA, RCBrace), GT)
              , ((RCBrace, RCBrace), GT)
              ]
  in M.fromList $ ords ++ generic ++ extra

  where filterterm :: (Op a -> Bool) -> Ordering -> [(a, Ordering)]
        filterterm f ord = zip (map term $ filter f ops) (repeat ord)

        genrel :: Op a -> [((Atom a, Atom a), Ordering)]
        genrel (RT . term -> rt) = [ ((rt, RA), LT)
                                  , ((RA, rt), GT)
                                  , ((rt, ROBrace), LT)
                                  , ((ROBrace, rt), LT)
                                  , ((RCBrace, rt), GT)
                                  , ((rt, RCBrace), GT)
                                  , ((rt, RS), GT)
                                  , ((RS, rt), LT)
                                  , ((RS, RS), EQ)
                                  ]

        rel :: Op a -> [(a, Ordering)]
        rel op =
          let
            -- infix and prefix
            lesser = filterterm (\op' -> fixty op' == Prefix || pri op < pri op') LT
            greater = filterterm (\op' -> fixty op' /= Prefix && pri op > pri op') GT
            eqord = case fixty op of
              Infixl -> GT
              Infixr -> LT
            -- infix only
            equal = if fixty op == Prefix then [] else filterterm (\op' -> fixty op == fixty op' && pri op == pri op') eqord
          in lesser ++ greater ++ equal

data Parsed a = POp (Atom a) [Parsed a]
              | Val a
              deriving (Show, Eq)

type OPState = State ([Parsed String], [Atom String]) ()

type OpCompare = [Op String] -> Atom String -> Atom String -> Ordering

opParser' :: OpCompare -> [Op String] -> [String] -> Parsed String
opParser' comp' ops str' =  head $ fst $ execState (mapM_ parse str' >> finish) ([], [RS])
  where comp = comp' ops

        lens :: Map (Atom String) Int
        lens = M.fromList $ map (\op -> (RT $ term op, if fixty op == Prefix then 1 else 2)) ops
               ++  [ (RCBrace, 1)
                   , (RA, 1)
                   ]

        reduceWhile :: Atom String -> OPState
        reduceWhile op = do
          (_, (op':_)) <- get
          case comp op' op of
           GT -> do
             modify $ second $ tail
             modify $ first $ \stk ->
               let (args, rest) = splitAt (lens M.! op') stk
               in (POp op' $ reverse args) : rest
             reduceWhile op
           EQ -> do
             modify $ second $ tail
           _ -> return ()

        parse :: String -> OPState
        parse c = do
          reduceWhile oc
          modify $ second (oc:)
          when (oc == RA) $ modify $ first (Val c:)

          where oc = case c of
                      "(" -> ROBrace
                      ")" -> RCBrace
                      _ | RT c `M.member` lens -> RT c
                        | otherwise -> RA

        finish :: OPState
        finish = reduceWhile RS

tableOp :: OpCompare
tableOp op a b = relationTable op M.! (a, b)

tableOpParser :: [Op String] -> String -> Parsed String
tableOpParser ops = opParser' tableOp ops . words
