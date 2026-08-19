module Laboratorio01 where

distanciaOrigen :: Double -> Double -> Double
distanciaOrigen x y = sqrt((x)^2 + (y)^2)

sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares xs = sum (map (^2) (filter even xs))

aplicaTresVeces :: (a -> a) -> a -> a
aplicaTresVeces f x = f (f (f x))

varianza2 :: Double -> Double -> Double

clasificaTemperatura :: Int -> String

intercala :: a -> [a] -> [a]

data Expr
  = Lit Int
  | Suma Expr Expr
  | Producto Expr Expr
  deriving (Eq, Show)

evalua :: Expr -> Int
