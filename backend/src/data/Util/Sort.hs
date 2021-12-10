-- |
-- Description : Util
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Monoid for sort
--
-- @since 0.3.0.0
module Util.Sort (toSort, getSorted) where

-- | 0.3.0.0
mergeSort :: Ord a => [a] -> [a] -> [a]
mergeSort xs [] = xs
mergeSort [] ys = ys
mergeSort xs'@(x : xs) ys'@(y : ys)
  | y < x = y : mergeSort xs' ys
  | otherwise = x : mergeSort xs ys'

-- | 0.3.0.0
newtype Sort a = Sort {getSorted :: [a]} deriving (Monoid, Functor, Applicative)

-- | 0.3.0.0
instance Ord a => Semigroup (Sort a) where
  (<>) = Sort <<$>> mergeSort `on` getSorted

-- | 0.3.0.0
toSort :: Ord a => [a] -> Sort a
toSort = foldMap (Sort . pure)
