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
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 1: desazucarado ----------------------------------------------------
-- YAHIR --
-- Convierte una lista no vacia de parametros distintos en funciones
-- unarias anidadas. El primer parametro queda en la funcion exterior.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _ = Nothing
curryFun [x] e = Just (Fun x e)
curryFun (x:xs) e
  | x `elem` xs = Nothing
  | otherwise =
      case curryFun xs e of
        Nothing     -> Nothing
        Just cuerpo -> Just (Fun x cuerpo)

-- Convierte una aplicacion con uno o mas argumentos en aplicaciones unarias
-- asociadas por la izquierda.
curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp e xs = Just (foldl App e xs)

--ISMAELLLLLLLL IMPLEMETANCION DE binaryOp and  desugar-----------------

-- Convierte dos o mas operandos en operaciones binarias asociadas por la
-- izquierda. El constructor recibido sera Add o Sub.
--por ejemplo Add [10,20,30] -> Add (Add 10 20) 30
--OJO: si o si debe recibir 2 ops[10] NO y [] NO
binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp op [] = Nothing
binaryOp op [x] = Nothing
binaryOp op [x,y] = Just (op x y)
binaryOp op (x:y:xs) = binaryOp op ([op x y] ++ xs)

-- Convierte las ligaduras de let* en let anidados y despues elimina cada let
-- mediante LetS x e1 e2 ==> App (Fun x e2') e1'. La primera ligadura debe
-- quedar en el let exterior para que las siguientes puedan usarla.
desugar :: SASA -> Maybe ASA
desugar (IdS x) = Just (Id x)
desugar (NumS n) = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)
desugar (NotS e) =
      case desugar e of
        Nothing -> Nothing
        Just e' -> Just (Not e')
desugar (AddS xs) =
    case desugarLista xs of
        Nothing -> Nothing
        Just xs' -> binaryOp Add xs'
desugar (SubS xs) =
    case desugarLista xs of
        Nothing -> Nothing
        Just xs' -> binaryOp Sub xs'
desugar (FunS params e) = 
      case desugar e of
        Nothing -> Nothing
        Just e' -> curryFun params e'

desugar (AppS f xs) = 
        case desugar f of
        Nothing -> Nothing
        Just f' ->         
          case desugarLista xs of
            Nothing -> Nothing
            Just xs'-> curryApp f' xs'
desugar (LetS x e1 e2)=
  case desugar e1 of
        Nothing -> Nothing
        Just e1' ->         
          case desugar e2 of
            Nothing -> Nothing
            Just e2'-> Just(App(Fun x e2') e1')
desugar (LetStarS bindings cuerpo) =
  desugar (letStarToLet bindings cuerpo)

--func aux para manejar la lista de SASA
desugarLista :: [SASA] -> Maybe [ASA]
desugarLista [] =Just[]
desugarLista (x:xs) = 
      case desugar x of
        Nothing -> Nothing
        Just x' -> 
                  case desugarLista xs of
                    Nothing -> Nothing
                    Just xs' -> Just(x' : xs')

--fUNC AUX PARA MANEJAR la lista de bindings en lets aninados
letStarToLet :: [(Nombre, SASA)] -> SASA -> SASA
letStarToLet [] cuerpo = cuerpo

letStarToLet ((x,e):xs) cuerpo =
    LetS x e (letStarToLet xs cuerpo)

-- RETO 2: evaluacion con cerraduras ---------------------------------------

-- --- Omar --- Implementacion del Reto 2

-- Busca la asociacion mas reciente de un identificador.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ [] = Nothing
lookupEnv nombre ((identificador, valor) : resto)
  | nombre == identificador = Just valor
  | otherwise = lookupEnv nombre resto

-- Evalua con alcance estatico. Fun produce una cerradura con el ambiente
-- actual. App evalua primero la posicion de funcion, despues el argumento y
-- por ultimo el cuerpo en el ambiente guardado por la cerradura.
-- La aplicacion es ansiosa: el argumento se exige aunque el cuerpo no lo use.
-- Conserva la resta truncada y la convencion de que todo numero cuenta como
-- verdadero cuando aparece como operando de Not.
bigStep :: Env -> ASA -> Maybe Value
bigStep ambiente expresion =
  case expresion of
    Id nombre -> lookupEnv nombre ambiente
    Num numero -> Just (NumV numero)
    Boolean booleano -> Just (BooleanV booleano)
    Add izquierda derecha -> do
      NumV valorIzquierdo <- bigStep ambiente izquierda
      NumV valorDerecho <- bigStep ambiente derecha
      pure (NumV (valorIzquierdo + valorDerecho))
    Sub izquierda derecha -> do
      NumV valorIzquierdo <- bigStep ambiente izquierda
      NumV valorDerecho <- bigStep ambiente derecha
      pure (NumV (max 0 (valorIzquierdo - valorDerecho)))
    Not operando -> do
      valor <- bigStep ambiente operando
      case valor of
        BooleanV booleano -> pure (BooleanV (not booleano))
        NumV _ -> pure (BooleanV False)
        ClosureV _ _ _ -> Nothing
    Fun parametro cuerpo -> Just (ClosureV parametro cuerpo ambiente)
    App funcion argumento -> do
      ClosureV parametro cuerpo ambienteCerradura <- bigStep ambiente funcion
      valorArgumento <- bigStep ambiente argumento
      bigStep ((parametro, valorArgumento) : ambienteCerradura) cuerpo
