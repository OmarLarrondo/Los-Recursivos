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
desugar (AddS es) = thenMaybe (sequenceMaybe (map desugar es)) (binaryOp Add)
desugar (SubS es) = thenMaybe (sequenceMaybe (map desugar es)) (binaryOp Sub)
desugar (NotS e) = mapMaybe1 Not (desugar e)
desugar (FunS xs e) = thenMaybe (desugar e) (curryFun xs)
desugar (AppS f args) =  thenMaybe (desugar f) (\f' -> thenMaybe (sequenceMaybe (map desugar args)) (curryApp f'))
desugar (IfS c t e) =  mapMaybe3 If (desugar c) (desugar t) (desugar e)
desugar (LetS x e1 e2) =  mapMaybe2 (\e1' e2' -> App (Fun x e2') e1') (desugar e1) (desugar e2)
desugar (LetRecS f e1 e2) =  desugar (LetS f (AppS (IdS "Y") [FunS [f] e1]) e2)
desugar (CondS cs elseE) = desugarCond cs elseE

-- RETO 4: evaluacion perezosa con alcance estatico ------------------------
-- [Autor: Omar]

-- Encadena una computacion Maybe con una segunda que depende de su
-- resultado. Es el eslabon entre cada regla del interprete. [Omar]
thenMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
thenMaybe Nothing _ = Nothing
thenMaybe (Just x) continuacion = continuacion x

-- Combina dos resultados Maybe con una funcion que puede fallar. [Omar]
seqMaybe2 :: Maybe a -> Maybe b -> (a -> b -> Maybe c) -> Maybe c
seqMaybe2 (Just x) (Just y) f = f x y
seqMaybe2 Nothing _ _ = Nothing
seqMaybe2 _ Nothing _ = Nothing

-- Busca la asociacion mas reciente sin exigir su contenido. [Omar]
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ [] = Nothing
lookupEnv x ((y, v) : resto)
  | x == y = Just v
  | otherwise = lookupEnv x resto

-- Exige una cerradura de expresion usando el ambiente guardado. Si al
-- evaluarla se obtiene otra ExprV, continua hasta producir otro valor. [Omar]
strict :: Value -> Maybe Value
strict (ExprV e env) = thenMaybe (bigStep env e) strict
strict v = Just v

-- Evalua una expresion y exige su resultado: bigStep seguido de strict.
-- Modela cada punto estricto del lenguaje. [Omar]
exige :: Env -> ASA -> Maybe Value
exige env e = thenMaybe (bigStep env e) strict

-- Semantica de paso grande con alcance estatico y evaluacion perezosa.
--
-- * Id devuelve directamente la asociacion encontrada.
-- * Fun produce ClosureV con el ambiente de definicion.
-- * App exige la posicion de funcion, pero liga el argumento como
--   ExprV argumento ambienteDeLaLlamada.
-- * Add, Sub y Not exigen sus operandos.
-- * If exige solamente la condicion y evalua una sola rama.
--
-- La resta sobre naturales permanece truncada en cero. [Omar]
bigStep :: Env -> ASA -> Maybe Value
bigStep env (Id x) = lookupEnv x env
bigStep _ (Num n) = Just (NumV n)
bigStep _ (Boolean b) = Just (BooleanV b)
bigStep env (Add e1 e2) = seqMaybe2 (exige env e1) (exige env e2) sumaV
bigStep env (Sub e1 e2) = seqMaybe2 (exige env e1) (exige env e2) restaV
bigStep env (Not e) = thenMaybe (exige env e) notV
bigStep env (Fun x b) = Just (ClosureV x b env)
bigStep env (App f a) = thenMaybe (exige env f) (\wf -> aplica wf env a)
bigStep env (If c t e) = thenMaybe (exige env c) (ifV env t e)

-- Suma dos naturales ya exigidos. [Omar]
sumaV :: Value -> Value -> Maybe Value
sumaV (NumV m) (NumV n) = Just (NumV (m + n))
sumaV _ _ = Nothing

-- Resta truncada en cero entre dos naturales ya exigidos. [Omar]
restaV :: Value -> Value -> Maybe Value
restaV (NumV m) (NumV n) = Just (NumV (max 0 (m - n)))
restaV _ _ = Nothing

-- Niegacion de un booleano ya exigido. [Omar]
notV :: Value -> Maybe Value
notV (BooleanV b) = Just (BooleanV (not b))
notV _ = Nothing

-- Aplica una cerradura de funcion. El argumento a no se evalua: se liga
-- como ExprV con el ambiente envLlamada de la llamada. El cuerpo se
-- evalua en el ambiente de definicion extendido (alcance estatico). [Omar]
aplica :: Value -> Env -> ASA -> Maybe Value
aplica (ClosureV p b envF) envLlamada a =
  bigStep ((p, ExprV a envLlamada) : envF) b
aplica _ _ _ = Nothing

-- Evalua unicamente la rama que la condicion, ya exigida, selecciona. [Omar]
ifV :: Env -> ASA -> ASA -> Value -> Maybe Value
ifV env t _ (BooleanV True) = bigStep env t
ifV env _ e (BooleanV False) = bigStep env e
ifV _ _ _ _ = Nothing
