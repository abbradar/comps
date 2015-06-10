{-# LANGUAGE ScopedTypeVariables #-}

import Data.Maybe
import Data.List
import Control.Arrow
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.State
import Debug.Trace

type Nonterm = String

data Atom a = NT Nonterm
            | T a
          deriving (Show, Eq, Ord)

type Expr a = [Atom a]

data Grammar a = Grammar { rules :: Map Nonterm [Expr a]
                         , start :: Nonterm
                         }
             deriving (Show, Eq)

data RAtom a = RT a
             | RS
             deriving (Show, Eq, Ord)

type PrecTable a = Map (RAtom a, RAtom a) Ordering

type RState a = State ([Nonterm], PrecTable a)

isPartOf :: Eq a => [a] -> [a] -> Bool
isPartOf [] _ = True
isPartOf _ [] = False
isPartOf sub lst@(_:t) = sub `isPrefixOf` lst || sub `isPartOf` t

-- Completely useless
relationTable :: forall a. (Show a, Eq a, Ord a) => Grammar a -> PrecTable a
relationTable gr = snd $ execState toplevel ([], M.empty)
  where toplevel :: RState a ()
        toplevel = do
          tnt Nothing $ start gr
          mapM_ (\b -> ins RS (RT b) LT) $ goodnt id $ start gr
          mapM_ (\a -> ins (RT a) RS GT) $ goodnt reverse $ start gr

        tnt :: Maybe Nonterm -> Nonterm -> RState a ()
        tnt parent nt = do
          (stk, _) <- get
          let (cycle, nstk) = span (/= nt) stk
              rep = (nt:cycle) `isPartOf` nstk
          unless (traceShow ("rep", nt, rep, cycle, nstk) rep) $ do
            modify $ traceShow ("stk", stk) $ first (nt:)
            forM_ (rules gr M.! nt) $ \rs -> do
              tr rs
              mapM_ (tnt $ Just nt) $ mapMaybe getnt rs
            modify $ first tail

        getnt :: Atom a -> Maybe Nonterm
        getnt (NT nt) = Just nt
        getnt _ = Nothing

        ins :: RAtom a -> RAtom a -> Ordering -> RState a ()
        ins a b ord = modify $ second $ M.insertWith resolve (a, b) ord
          where resolve o1 o2
                  | o1 == o2 = o1
                  | otherwise = error "relationTable: cannot construct relation table"

        ins' :: a -> a -> Ordering -> RState a ()
        ins' a b = ins (RT a) (RT b)

        goodnt :: (Expr a -> Expr a) -> Nonterm -> RState a [a]
        goodnt f nt = let rs = map f $ rules gr M.! nt
                      in mapMaybe test rs ++ concatMap nextnt rs
          where test ((T x):_) = Just x
                test ((NT _):(T x):_) = Just x
                test _ = Nothing
                nextnt ((NT nt'):_) | nt /= nt' = goodnt f nt'
                nextnt _ = []

        tr :: (Expr a) -> RState a ()
        tr ((T a):(T b):t) = ins' a b EQ >> tr t
        tr ((T a):(NT nt):(T b):t) = do
          mapM_ (\b -> ins' a b LT) $ goodnt id nt
          ins' a b EQ
          mapM_ (\a -> ins' a b GT) $ goodnt reverse nt
          tr t
        tr ((T a):(NT nt):t) = do
          mapM_ (\b -> ins' a b LT) $ goodnt id nt
          tr t
        tr ((NT nt):(T b):t) = do
          mapM_ (\a -> ins' a b GT) $ goodnt reverse nt
          tr t
        tr (_:t) = tr t
        tr [] = return ()
