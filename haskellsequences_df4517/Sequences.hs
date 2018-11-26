module Sequences where

import Data.Char (ord, chr)

maxOf2 :: Int -> Int -> Int
maxOf2 x y
     | x > y     = x -- Returns first argument if it is larger than the second
     | otherwise = y -- the second argument otherwise

maxOf3 :: Int -> Int -> Int -> Int
-- Returns the largest of three Ints
maxOf3 x y z
  | x > maxOf2 y z  = x
  | otherwise       = maxOf2 y z

isADigit :: Char -> Bool
isADigit a
  | ord a <= ord '9' && ord a >= ord '0' = True
  -- Returns True if the character represents a digit '0'..'9';
  | otherwise = False
  -- False otherwise

isAlpha :: Char -> Bool
-- Returns True if the character represents an alphabetic
-- character either in the range 'a'..'z' or in the range 'A'..'Z';
isAlpha a
  | (ord a >= ord 'a' && ord a <= ord 'z')
    || (ord a >= ord 'A' && ord a <= ord 'Z') = True
  | otherwise = False

digitToInt :: Char -> Int
-- Pre: the character is one of '0'..'9'
-- Returns the integer [0..9] corresponding to the given character.
digitToInt b
  | isADigit b = ord b - ord '0'
  | otherwise = error (b:" is not a valid digit!")
  -- Note: this is a simpler version of digitToInt in module Data.Char,
  -- which does not assume the precondition.

toUpper :: Char -> Char
toUpper c
  | ord c >= ord 'a' && ord c <= ord 'z' = chr (ord c - 32)
  | isAlpha c = c
  | otherwise = error(c:" is not a valid letter")
  -- Uses guards by way of variety.

--
-- Sequences and series
--

-- Arithmetic sequence
arithmeticSeq :: Double -> Double -> Int -> Double
arithmeticSeq a d n = a + fromIntegral n * d

-- Geometric sequence
geometricSeq :: Double -> Double -> Int -> Double
geometricSeq a r n = a*r^fromIntegral n

-- Arithmetic series
arithmeticSeries :: Double -> Double -> Int -> Double
arithmeticSeries a d n = (fromIntegral (n + 1)) * (a + d * 0.5 * fromIntegral n)

-- Geometric series
geometricSeries :: Double -> Double -> Int -> Double
geometricSeries a r n
  | r == 1 = a * (fromIntegral (n + 1))
  | otherwise = a * (1 - (r ^ fromIntegral (n + 1))) / (1 - r)
