{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wall -Werror #-}
-- Needed for adding instances outside the data definition module.
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW5 where

import Control.Applicative (liftA2)
import Control.Monad
import Data.Either
import Data.Foldable
import Data.List (foldl', uncons)
import Data.Map (Map, (!?))
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (All (..), Any (..), First (..), Last (..), Product (..), Sum (..))
import Data.Ord (Down (..))
import Data.Semigroup (Arg (..), Max (..), Min (..))
import Data.Set (Set)
import qualified Data.Set as S
import Prelude (Applicative (..), Bool (..), Char, Either (..), Enum (..), Eq (..), FilePath, Foldable (foldMap, foldl, foldr), Functor (fmap), IO, Int, Maybe (..), Monad (..), Monoid (..), Num (..), Ord (..), Ordering (..), Semigroup (..), Show (..), String, all, and, any, concat, concatMap, const, curry, div, drop, dropWhile, error, filter, flip, fst, getLine, id, init, lines, map, not, or, otherwise, putStrLn, readFile, replicate, reverse, sequenceA, snd, take, takeWhile, traverse, uncurry, undefined, unlines, writeFile, zip, zipWith, (!!), ($), (&&), (++), (.), (<$>), (||))

import Calculator
import Deque (Deque)
import qualified Deque as DQ
import State


-- Section 1
data NonEmpty a = a :| [a] deriving (Show, Eq, Ord, Functor, Foldable)
instance Semigroup (NonEmpty a) where
    (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
    (x :| xs) <> (y :| ys) = x :| (xs ++ y : ys)

instance Applicative NonEmpty where
    liftA2 :: (t -> a -> b) -> NonEmpty t -> NonEmpty a -> NonEmpty b
    liftA2 f fa fb = fa >>= (\a -> f a <$> fb)
    pure :: a -> NonEmpty a
    pure = return

instance Monad NonEmpty where
    (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
    (>>=) (a :| as) f = case as of
        [] -> f a
        (x :xs) -> f a <> ((x :| xs) >>= f)
    return :: a -> NonEmpty a
    return x = x :| []

instance Applicative Deque where
    liftA2 :: (t -> a -> b) -> Deque t -> Deque a -> Deque b
    liftA2 f fa fb = fa >>= (\a -> f a <$> fb)
    pure :: a -> Deque a
    pure = return

instance Monad Deque where
    return :: a -> Deque a
    return x = DQ.pushl x DQ.empty
    (>>=) :: Deque a -> (a -> Deque b) -> Deque b
    (>>=) q f = case DQ.popl q of
        Nothing -> DQ.empty
        Just (x, q') -> f x <> (q' >>= f)


-- Section 2
joinGrades :: FilePath -> FilePath -> FilePath -> IO ()
joinGrades = undefined
-- Section 3
guessingGame :: Int -> Int -> IO Int
guessingGame = undefined

-- Section 4
data Result = Result
    { finalValues :: Map String Int
    , missingVariables :: Map String Int
    , divisionByZero :: Int
    }
    deriving (Show, Eq)
runCalculator :: [(String, Expression)] -> Result
runCalculator = undefined
