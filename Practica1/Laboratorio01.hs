module Laboratorio01 where

distanciaOrigen :: Double -> Double -> Double
distanciaOrigen x y = sqrt((x)^2 + (y)^2)

sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares xs = sum (map (^2) (filter even xs))

aplicaTresVeces :: (a -> a) -> a -> a
aplicaTresVeces f x = f (f (f x))

--ISMAEL--
varianza2 :: Double -> Double -> Double
varianza2 0 0 = 0
varianza2 x y =
    let media = varianza_media x y
    in (x - media) ^ 2+ (y - media ) ^ 2 / 2

varianza_media :: Double -> Double -> Double
varianza_media x y = (x + y)/2


clasificaTemperatura :: Int -> String

intercala :: a -> [a] -> [a]
intercala _ [] = []
intercala _ [x] = [x]
intercala sep (x : xs) = x : sep : intercala sep xs

data Expr
  = Lit Int
  | Suma Expr Expr
  | Producto Expr Expr
  deriving (Eq, Show)

evalua :: Expr -> Int
evalua (Lit n) = n
evalua (Suma e1 e2) = evalua e1 + evalua e2
evalua (Producto e1 e2) = evalua e1 * evalua e2
