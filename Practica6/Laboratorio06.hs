module Laboratorio06 where

-- 1. Potencia --------------------------------------------------------------

-- Precondicion: el exponente es mayor o igual que cero.
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia base exponente =
  base * potencia base (exponente - 1)

potenciaCola :: Int -> Int -> Int
potenciaCola base exponente = potenciaColaAux base exponente 1
  where
    potenciaColaAux :: Int -> Int -> Int -> Int
    potenciaColaAux _ 0 acc = acc
    potenciaColaAux b e acc = potenciaColaAux b (e - 1) (acc * b)

potenciaCPS :: Int -> Int -> (Int -> r) -> r
potenciaCPS _ 0 k = k 1
potenciaCPS base exponente k = 
  potenciaCPS base (exponente - 1) (\resultado -> k (base * resultado))

-- 2. Longitud de una lista -------------------------------------------------

longitud :: [a] -> Int
longitud [] = 0
longitud (_ : xs) = 1 + longitud xs

longitudCola :: [a] -> Int
longitudCola xs = longitudColaAux xs 0
  where
    longitudColaAux :: [a] -> Int -> Int
    longitudColaAux [] acc = acc
    longitudColaAux (_ : xs) acc = longitudColaAux xs (acc + 1)

longitudCPS :: [a] -> (Int -> r) -> r
longitudCPS [] k = k 0
longitudCPS (_ : xs) k = longitudCPS xs (\n -> k (1 + n))

-- 3. Reversa de una lista --------------------------------------------------

reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x]

reversaCola :: [a] -> [a]
reversaCola xs = reversaColaAux xs []
  where
    reversaColaAux :: [a] -> [a] -> [a]
    reversaColaAux [] acc = acc
    reversaColaAux (x : xs) acc = reversaColaAux xs (x : acc)

reversaCPS :: [a] -> ([a] -> r) -> r
reversaCPS [] k = k []
reversaCPS (x : xs) k =
  reversaCPS xs (\res -> k (res ++ [x]))

-- 4. Concatenacion de una lista de listas ---------------------------------

concatena :: [[a]] -> [a]
concatena [] = []
concatena (xs : xss) = xs ++ concatena xss

concatenaCola :: [[a]] -> [a]
concatenaCola xss = concatenaColaAux (reversaCola xss) []
  where
    concatenaColaAux :: [[a]] -> [a] -> [a]
    concatenaColaAux [] acc = acc
    concatenaColaAux (xs : xss') acc = concatenaColaAux xss' (xs ++ acc)

concatenaCPS :: [[a]] -> ([a] -> r) -> r
concatenaCPS [] k = k []
concatenaCPS (xs : xss) k =
  concatenaCPS xss (\res -> k (xs ++ res))

-- 5. Intercalado de dos listas --------------------------------------------

intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x : xs) (y : ys) =
  x : y : intercala xs ys

intercalaCola :: [a] -> [a] -> [a]
intercalaCola xs ys = reversaCola (intercalaColaAux xs ys [])
  where
    intercalaColaAux :: [a] -> [a] -> [a] -> [a]
    intercalaColaAux [] ys acc = intercalaColaFin ys acc
    intercalaColaAux xs [] acc = intercalaColaFin xs acc
    intercalaColaAux (x : xs) (y : ys) acc =
      intercalaColaAux xs ys (y : x : acc)

    intercalaColaFin :: [a] -> [a] -> [a]
    intercalaColaFin [] acc = acc
    intercalaColaFin (z : zs) acc = intercalaColaFin zs (z : acc)

intercalaCPS :: [a] -> [a] -> ([a] -> r) -> r
intercalaCPS [] ys k = k ys
intercalaCPS xs [] k = k xs
intercalaCPS (x : xs) (y : ys) k =
  intercalaCPS xs ys (\resto -> k (x : y : resto))
