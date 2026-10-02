module Interp where

import Grammars

data ASA
  = Id Nombre
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | Fun Nombre ASA
  | App ASA ASA
  | If ASA ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  | ExprV ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 3: desazucarado ----------------------------------------------------

-- FUNCIONES AUXILIARES --
-- Convierte [Maybe a] en Maybe [a]: solo si todos son Just.
-- Si alguno es Nothing, devuelve Nothing.
sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe [] = Just []
sequenceMaybe (Just x : rest) = consMaybe x (sequenceMaybe rest)
sequenceMaybe (Nothing : _) = Nothing

consMaybe :: a -> Maybe [a] -> Maybe [a]
consMaybe _ Nothing = Nothing
consMaybe x (Just xs) = Just (x : xs)

-- Aplica una funcion de un argumento a un Maybe.
mapMaybe1 :: (a -> b) -> Maybe a -> Maybe b
mapMaybe1 _ Nothing = Nothing
mapMaybe1 f (Just x) = Just (f x)

-- Combina dos Maybe con una funcion binaria.
mapMaybe2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
mapMaybe2 _ Nothing _ = Nothing
mapMaybe2 _ _ Nothing = Nothing
mapMaybe2 f (Just x) (Just y) = Just (f x y)

-- Combina tres Maybe con una funcion ternaria.
mapMaybe3 :: (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
mapMaybe3 _ Nothing _ _ = Nothing
mapMaybe3 _ _ Nothing _ = Nothing
mapMaybe3 _ _ _ Nothing = Nothing
mapMaybe3 f (Just x) (Just y) (Just z) = Just (f x y z)

-- Recupera estas funciones del laboratorio 4. Las funciones y aplicaciones
-- del nucleo siguen siendo unarias, y las operaciones siguen siendo binarias.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _ = Nothing
curryFun [x] e = Just (Fun x e)
curryFun (x:xs) e
  | x `elem` xs = Nothing
  | otherwise   = mapMaybe1 (Fun x) (curryFun xs e)

curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp e xs = Just (foldl App e xs)

binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp op [] = Nothing
binaryOp op [x] = Nothing
binaryOp op [x,y] = Just (op x y)
binaryOp op (x:y:xs) = binaryOp op ([op x y] ++ xs)

-- Desazucara las clausulas ordinarias de cond en If anidados. La alternativa
-- else es el ultimo argumento y se conserva como la rama final.
desugarCond :: [(SASA, SASA)] -> SASA -> Maybe ASA
desugarCond [] e = desugar e
desugarCond ((c, e):cs) elseE =
  mapMaybe3 (\c' e' rest -> If c' e' rest)
            (desugar c)
            (desugar e)
            (desugarCond cs elseE)

-- Elimina toda la sintaxis superficial. CondS se traduce a If anidados.
-- LetRecS f definicion cuerpo se traduce usando el identificador Y:
--
--   LetS f (AppS (IdS "Y") (FunS [f] definicion)) cuerpo
--
-- y despues se elimina tambien ese LetS. LetRecS no pertenece al nucleo.
desugar :: SASA -> Maybe ASA
desugar (IdS x) = Just (Id x)
desugar (NumS n) = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)
desugar (AddS es) = mapMaybe1 (binaryOp Add) (sequenceMaybe (map desugar es))
desugar (SubS es) = mapMaybe1 (binaryOp Sub) (sequenceMaybe (map desugar es))
desugar (NotS e) = mapMaybe1 Not (desugar e)
desugar (FunS xs e) = mapMaybe1 (curryFun xs) (desugar e)
desugar (AppS f args) =  mapMaybe2 curryApp (desugar f) (sequenceMaybe (map desugar args))
desugar (IfS c t e) =  mapMaybe3 If (desugar c) (desugar t) (desugar e)
desugar (LetS x e1 e2) =  mapMaybe2 (\e1' e2' -> App (Fun x e2') e1') (desugar e1) (desugar e2)
desugar (LetRecS f e1 e2) =  desugar (LetS f (AppS (IdS "Y") (FunS [f] e1)) e2)
desugar (CondS cs elseE) = desugarCond cs elseE

-- RETO 4: evaluacion perezosa con alcance estatico ------------------------

-- Busca la asociacion mas reciente sin exigir su contenido.
lookupEnv :: Nombre -> Env -> Maybe Value

-- Exige una cerradura de expresion usando el ambiente guardado. Si al
-- evaluarla se obtiene otra ExprV, continua hasta producir otro valor.
strict :: Value -> Maybe Value

-- Semantica de paso grande con alcance estatico y evaluacion perezosa.
--
-- * Id devuelve directamente la asociacion encontrada.
-- * Fun produce ClosureV con el ambiente de definicion.
-- * App exige la posicion de funcion, pero liga el argumento como
--   ExprV argumento ambienteDeLaLlamada.
-- * Add, Sub y Not exigen sus operandos.
-- * If exige solamente la condicion y evalua una sola rama.
--
-- La resta sobre naturales permanece truncada en cero.
bigStep :: Env -> ASA -> Maybe Value
