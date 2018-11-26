module Crypto where

import Data.Char

import Prelude hiding (gcd)

{-
The advantage of symmetric encryption schemes like AES is that they are efficient
and we can encrypt data of arbitrary size. The problem is how to share the key.
The flaw of the RSA is that it is slow and we can only encrypt data of size lower
than the RSA modulus n, usually around 1024 bits (64 bits for this exercise!).

We usually encrypt messages with a private encryption scheme like AES-256 with
a symmetric key k. The key k of fixed size 256 bits for example is then exchanged
via the aymmetric RSA.
-}

-------------------------------------------------------------------------------
-- PART 1 : asymmetric encryption

gcd :: Int -> Int -> Int
gcd m n = if n == 0 then m else (gcd n (mod m n))

phi :: Int -> Int
phi m = length [ x | x <- [1..m], gcd m x == 1]

extendedGCD :: Int -> Int -> ((Int, Int), Int)
extendedGCD a b
  | b == 0 = ((1, 0), a)
  | otherwise = ((v', (u' - q * v')), gcd b (mod a b))
  where
    (q, _r) = quotRem a b
    ((u', v'), _d) = extendedGCD b (mod a b)

inverse :: Int -> Int -> Int
inverse a m = mod (fst (fst (extendedGCD a m))) m

modPow :: Int -> Int -> Int -> Int
modPow a k m
  | k == 0 = mod 1 m
  | even k = let j = div k 2 in mod((mod expr m) * (modPow expr (j - 1) m)) m
  | otherwise = let j = div (k - 1) 2 in mod (a * modPow expr j m) m
  where
    expr = mod (a ^ 2) m

smallestCoPrimeOf :: Int -> Int
smallestCoPrimeOf phi = head [ x | x <- [2..], gcd phi x == 1]

genKeys :: Int -> Int -> ((Int, Int), (Int, Int))
genKeys p q
  = ((e, n), (d, n))
  where
    n = p * q
    expr = (p - 1) * (q - 1)
    e = smallestCoPrimeOf expr
    d = inverse e expr

rsaEncrypt :: Int -> (Int, Int) -> Int
rsaEncrypt m (e, n)
  = modPow m e n

rsaDecrypt :: Int -> (Int, Int) -> Int
rsaDecrypt = rsaEncrypt

-------------------------------------------------------------------------------
-- PART 2 : symmetric encryption

-- Returns position of a letter in the alphabet
toInt :: Char -> Int
toInt c
  | ordc >= orda && ordc <= ord 'z' = ordc - orda
  | ordc >= ordA && ordc <= ord 'Z' = ordc - ordA
  | otherwise = error"Not a letter"
  where
    ordc = ord c
    orda = ord 'a'
    ordA = ord 'A'

toChar :: Int -> Char
-- Pre: n>=0 && n <= 25
toChar n
  = chr (n + ord 'a')

add :: Char -> Char -> Char
add a b
  = toChar (mod (toInt a + toInt b) 26)

substract :: Char -> Char -> Char
substract a b
  = toChar (mod (toInt a - toInt b) 26)

-- the next functions present
-- 2 modes of operation for block ciphers : ECB and CBC
-- based on a symmetric encryption function e/d such as "add"

-- ecb (electronic codebook) with block size of a letter
--
ecbEncrypt :: Char -> String -> String
ecbEncrypt key
  = map (add key)

ecbDecrypt :: Char -> String -> String
ecbDecrypt key
  = map (flip substract key)

-- cbc (cipherblock chaining) encryption with block size of a letter
-- initialisation vector iv is a letter
-- last argument is message m as a string
--
cbcEncrypt :: Char -> Char -> String -> String
cbcEncrypt _ _ []
  = []
cbcEncrypt key iv (m : ms)
  = current : (cbcEncrypt key current ms)
  where
    current = (add (add m iv) key)

cbcDecrypt :: Char -> Char -> String -> String
cbcDecrypt key iv word
  = reverse (cbcDecrypt' (reverse word))
  where
    cbcDecrypt' []
      = []
    cbcDecrypt' (m : [])
      = [substract (substract m iv) key]
    cbcDecrypt' (m : m' : ms)
      = (substract (substract m m') key) : (cbcDecrypt' (m' : ms))
