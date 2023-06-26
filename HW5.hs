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
import System.Posix (fileSize)
import Distribution.Simple.Utils (xargs)
import Data.Time.Format.ISO8601 (yearFormat)


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
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f (Just x) = Just $ f x
maybeMap _ _ = Nothing

splitOn :: Char -> String -> Maybe (String, String)
splitOn _ [] = Nothing
splitOn c (x : xs) | c == x = Just ([], xs)
splitOn c (x : xs) = maybeMap (\ (p, s) -> (x : p, s)) $ splitOn c xs

getMap :: String -> Map String String
getMap input = M.fromList $ map (\g -> fromMaybe ("","") (splitOn ',' (filter (/=' ') g))) (lines input)

joinGrades :: FilePath -> FilePath -> FilePath -> IO ()
joinGrades groupsFile gradesFile outputFile = do
    groupsRaw <- readFile groupsFile
    gradesRaw <- readFile gradesFile
    let groupsMap = getMap groupsRaw
    let gradesMap = getMap gradesRaw
    let outputMap = M.map (\v -> fromMaybe "0" (M.lookup v gradesMap)) groupsMap
    let outputRaw = unlines $ map (\(a,b) -> a ++ ", " ++ b) (M.toList outputMap)
    writeFile outputFile outputRaw

-- joinGrades "groups.txt" "grades.txt" "output.txt"
    

-- Section 3
guessingGame :: Int -> Int -> IO Int
guessingGame x y = 
    if x == y then return x else 
    do
    let guess = x + div (y-x) 2
    putStrLn ("Is the number less than , equal , or greater than " ++ show guess ++ "? (l/e/g)")
    answer <- getLine
    case answer of
        "l" -> guessingGame x guess
        "e" -> return guess
        "g" -> guessingGame guess y
        _ -> guessingGame x y

-- Section 4
data Result = Result
    { finalValues :: Map String Int
    , missingVariables :: Map String Int
    , divisionByZero :: Int
    }
    deriving (Show, Eq)
runCalculator :: [(String, Expression)] -> Result
runCalculator = undefined
