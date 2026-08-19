module Laboratorio01 where

distanciaOrigen :: Double -> Double -> Double

sumaCuadradosPares :: [Int] -> Int

aplicaTresVeces :: (a -> a) -> a -> a

varianza2 :: Double -> Double -> Double

clasificaTemperatura :: Int -> String

intercala :: a -> [a] -> [a]

data Expr
  = Lit Int
  | Suma Expr Expr
  | Producto Expr Expr
  deriving (Eq, Show)

evalua :: Expr -> Int
