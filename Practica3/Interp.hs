module Interp where

import Grammars


----------------------------------------------
----------------ISMAEL---------------------------
------------------------------
-- RETO 3: sustitucion nominal que evita captura
freeVars :: ASA -> [String]
freeVars = nub . freeVars'

--regresamos la vacia, pues no tiene variables aqui
freeVars' (Num n) = []
freeVars' (Boolean _) = []

--el Id es que es una var libre, pues no la esta agarrando nada
freeVars' (Id x) = [x]

--con varios elementos el ASA
--se debe sacar las varsfree de cada elemento de la lista xs
-- y se concatena con su cola
freeVars' (And xs) = freeVarsLista xs
freeVars' (Or xs) =  freeVarsLista xs
freeVars' (Add xs) =  freeVarsLista xs
freeVars' (Sub xs) =  freeVarsLista xs
freeVars' (Mul xs) =  freeVarsLista xs
freeVars' (Div xs) =  freeVarsLista xs
freeVars' (Lt xs) =  freeVarsLista xs
freeVars' (Gt xs) =  freeVarsLista xs
freeVars' (Le xs) =  freeVarsLista xs
freeVars' (Ge xs) =  freeVarsLista xs

--con un solo arg
--se pasa directo, pues las vars libres de estas son las mismas
freeVars' (Not x) =  freeVars' x
freeVars' (Add1 x) =  freeVars' x 
freeVars' (Sub1 x) =  freeVars' x 
freeVars' (ZeroP x) =  freeVars' x

--ops con 2 ASA 
--lo mismo 
freeVars' (Expt x y ) =  freeVars' x ++ freeVars' y
freeVars' (EqP x y ) =  freeVars' x ++ freeVars' y

--let y letStar...
--OJOOOO: binding es la dupla (String, ASA) == (nombre, expresion)
-- por ejemplo es [("x", Num 10)]
--let los nombres ligan solo el cuerpo

--primero analizar las vars libres de la expresion, luego el cuerpo

freeVars' (Let bindings cuerpo) =
    let nombres = nameStringBind bindings
        varsEnExprs = freeVarsBindings bindings
        varsEnCuerpo = varsLibresCuerpo cuerpo
        --SE USA FILTER, PUES \\ SOLO ELIMINA LA PRIMERA APARICION
        --CON FILTER CHECA CADA UNO Y LOS ELIMINA
    in nub (varsEnExprs ++ filter (`notElem` nombres) varsEnCuerpo)
--caso donde no ha bindings
freeVars' (LetStar [] cuerpo ) = freeVars' cuerpo

--caso interesnate:
freeVars' (LetStar (b:bs) cuerpo) =
    freeVars' (Let [b] (LetStar bs cuerpo))

--func aux
--saca las vars libres de (String, expresion)
freeVarsBindings :: [Binding] -> [String]
freeVarsBindings [] = []
freeVarsBindings ((y, expr):xs) = freeVarsExpBind (y, expr) ++ freeVarsBindings xs

--saca las vars libres de un bing
--variables libres de las expresiones asignadas en los bindings (los ASA de la tupla).
freeVarsExpBind :: (String,ASA) -> [String]
freeVarsExpBind (name, expr) = freeVars' expr

--nombres declarados (los String de la tupla).
nameStringBind :: [(String, ASA)] -> [String]
nameStringBind [] = []
nameStringBind ((x,_):xs) = [x] ++ nameStringBind xs

--vars libres del cuerpo
varsLibresCuerpo :: ASA ->[String]
varsLibresCuerpo x = freeVars x


--func auxiliar para sacar las vars libres (no fue tan necesaria pues e puso usar funciones de ordne sup)
freeVarsLista :: [ASA] -> [String]
freeVarsLista [] = []
freeVarsLista (x:xs) = freeVars' x ++freeVarsLista xs

--agarrar todos los names del arbol, vars libres, ligadas y los names
--declarados en los bingings
names :: ASA -> [String]
names = nub . names'
names' (Num n) = []
names' (Boolean _) = []
names' (Id x) = [x]
names' (And xs) = namesLista xs
names' (Or xs) =  namesLista xs
names' (Add xs) = namesLista xs
names' (Sub xs) = namesLista xs
names' (Mul xs) = namesLista xs
names' (Div xs) = namesLista xs
names' (Lt xs) =  namesLista xs
names' (Gt xs) =  namesLista xs
names' (Le xs) =  namesLista xs
names' (Ge xs) =  namesLista xs

names' (Not x) =  names' x
names' (Add1 x) =  names' x 
names' (Sub1 x) =  names' x 
names' (ZeroP x) =  names' x
names' (Expt x y ) =  names' x ++ names' y
names' (EqP x y ) =  names' x ++ names' y

--names en (String, ASA) y en el cuerpo igual es un ASA

names' (Let bindings cuerpo ) =  
    let nombresBinding = namesBindings bindings
        nameCuerpo = names' cuerpo
    in nub (nombresBinding ++ nameCuerpo)
--caso interesnate:
names' (LetStar bindings cuerpo) = 
    nub (namesBindings bindings ++ names cuerpo)



--func auxiliar para sacar los names
namesLista :: [ASA] -> [String]
namesLista [] = []
namesLista (x:xs) = names' x ++ namesLista xs


-- Extrae TODOS los nombres presentes en los bindings (tanto la clave String como el cuerpo ASA)
namesBindings :: [Binding] -> [String]
namesBindings [] = []
namesBindings ((x, expr) : xs) = x : names expr ++ namesBindings xs

--[z1,z2,...] infinitamente hasta que sea el bueno
freshName :: [String] -> String
freshName xs = nuevoNombre "z" xs
  where
    nuevoNombre base ocupados = head [n | n <- candidatos, not (n `elem` ocupados)]
      where candidatos = base : [base ++ show i | i <- [1..]]


sust :: ASA -> String -> ASA -> ASA
sust (Id y) x s 
    | y == x = s
    | otherwise = (Id y)
sust (Num n) x s = Num n
sust (Boolean b) x s = Boolean b

--n ops
sust (And ys) x s =  And (sustLista ys x s)
sust (Or ys) x s =  Or (sustLista ys x s)
sust (Add ys) x s =  Add (sustLista ys x s)
sust (Sub ys) x s =  Sub (sustLista ys x s)
sust (Mul ys) x s =  Mul (sustLista ys x s)
sust (Div ys) x s =  Div (sustLista ys x s)
sust (Lt ys) x s =  Lt (sustLista ys x s)
sust (Gt ys) x s =  Gt (sustLista ys x s)
sust (Le ys) x s =  Le (sustLista ys x s)
sust (Ge ys) x s =  Ge (sustLista ys x s)
--1 argumen
sust (Not y) x s =  Not (sust y x s)
sust (Add1 y) x s =  Add1 (sust y x s)
sust (Sub1 y) x s =  Sub1 (sust y x s)
sust (ZeroP y) x s =  ZeroP (sust y x s)
--2 argu
sust (Expt y z) x s =  Expt (sust y x s) (sust z x s)
sust (EqP y z) x s =  EqP (sust y x s) (sust z x s)

-- let y letStar
sust (Let bindings cuerpo) x s
    --si x ya esta ligada por este let, no se sustituye en el cuerpo
    --solo sustituye en las expre de los binding, el cuerpo no se toca
    | x `elem` nameStringBind bindings = Let (sustBindingsExprs bindings x s) cuerpo
    | otherwise =
        --checar captura de vars libres, puede que en let al momento de 
        --sustiuir las vars que no eran libres, ahora estne ligadas
        --evita captura detecta si algun name de bindings choca con las vars libres de s
        --si choca, crea una vars nueva para renombrar la var 
        let (bindingsRenombrados, cuerpoRenombrado) = evitaCaptura bindings cuerpo s
            --hace la sustitucion
            bindingsFinales = sustBindingsExprs bindingsRenombrados x s
            --usa cuerpoRenombrado para sustituir
            cuerpoFinal = sust cuerpoRenombrado x s
        --hace el Let correcto
        in Let bindingsFinales cuerpoFinal

--letStar sin bindings
--si no hay bindings, se sustituye directamente en el cuerpo
sust (LetStar [] cuerpo) x s = sust cuerpo x s

--letStar con varios bindings
--se convierte en lets normales, igual que en freeVars
--letStar [y1,y2,.] cuerpo == let [y1] (letStar [ys,...] cuerpo)
--anidacion de let s normales 
sust (LetStar bindings cuerpo) x s =
    let (bindingsFinales, cuerpoFinal) = sustLetStarAux bindings cuerpo x s
    in LetStar bindingsFinales cuerpoFinal

-- Procesa los bindings uno a uno
sustLetStarAux :: [Binding] -> ASA -> String -> ASA -> ([Binding], ASA)
sustLetStarAux [] cuerpo x s = ([], sust cuerpo x s)
sustLetStarAux ((nombre, expr):bs) cuerpo x s
    -- vuelve a ligar x, se sustituye SOLO su propio expr
    | nombre == x =
        ((nombre, sust expr x s) : bs, cuerpo)
    -- aparece libre en s
    | nombre `elem` freeVars s =
        let prohibidos = names s ++ names cuerpo ++ concatMap (\(n,e) -> n : names e) bs
            --generar un name nuevo
            nombreFresco = freshName prohibidos
            --quita las vars viejas en las expr de los bindig
            bsRenombrado = renombraBindings bs nombre nombreFresco
            cuerpoRenombrado = sust cuerpo nombre (Id nombreFresco)
            (bsFinal, cuerpoFinal) = sustLetStarAux bsRenombrado cuerpoRenombrado x s
            --continuar sustiruyendo x
        in ((nombreFresco, sust expr x s) : bsFinal, cuerpoFinal)
    -- si el name de binginf no es x y tampco causa captura con s,
    -- sustituye x en la expr actual

    | otherwise =
        let (bsFinal, cuerpoFinal) = sustLetStarAux bs cuerpo x s
        in ((nombre, sust expr x s) : bsFinal, cuerpoFinal)

-- busca las expr que vienen de abajo para cambiarlas a nuevas
renombraBindings :: [Binding] -> String -> String -> [Binding]
renombraBindings [] _ _ = []
renombraBindings ((nombre, expr):bs) viejo nuevo
    | nombre == viejo = (nombre, sust expr viejo (Id nuevo)) : bs
    | otherwise = (nombre, sust expr viejo (Id nuevo)) : renombraBindings bs viejo nuevo

--func auxiliar
--sustituye en las expresiones de los bindings
--el nombre del binding no se modifica aqui
sustBindingsExprs :: [Binding] -> String -> ASA -> [Binding]
sustBindingsExprs [] x s = []

sustBindingsExprs ((nombre, expr):bs) x s =
    (nombre, sust expr x s) : sustBindingsExprs bs x s
--aux
sustLista :: [ASA] -> String -> ASA -> [ASA]
sustLista [] x s = []
sustLista (y:ys) x s = [sust y x s] ++ sustLista ys x s  

--func auxiliar
--revisa si algun nombre de los bindings puede capturarz
--una variable libre de s
evitaCaptura :: [Binding] -> ASA -> ASA -> ([Binding], ASA)
evitaCaptura [] cuerpo s = ([], cuerpo)
evitaCaptura ((nombre, expr):bs) cuerpo s
    --si el nombre del binding aparece como variable libre en s
    --puede capturarla, entonces hay que cambiarle el nombre con freshvars
    | nombre `elem` freeVars s =
        let prohibidos = names s ++ names cuerpo ++ map fst bs
            nombreFresco = freshName prohibidos
            --se cambia por una nombre nuevo solo en el cuerpo
            cuerpoRenombrado = sust cuerpo nombre (Id nombreFresco)
            --se revisa los demas binding, pero ahora con el cuerpo nuevo y se hace lo mismo
            (bsRestantes, cuerpoFinal) =
                evitaCaptura bs cuerpoRenombrado s
        in ((nombreFresco, expr) : bsRestantes, cuerpoFinal)
    --no hay problemas, entonces se sigue con los otros a ver si tiene problemas los bs
    | otherwise =
        let (bsRestantes, cuerpoFinal) =
                evitaCaptura bs cuerpo s
        in ((nombre, expr) : bsRestantes, cuerpoFinal)


sustMany :: ASA -> [Binding] -> ASA
sustMany cuerpo [] = cuerpo
sustMany cuerpo bindings =
    --separar los names 
    let xs = map fst bindings
        ss = map snd bindings
        -- sacar las vars no libres
        ocupados = names cuerpo ++ concatMap freeVars ss ++ xs
        -- generar vars nuevas
        frescos = refrescarNombres xs ocupados
        --renombramos las variables del cuerpo con los nombres frescos
        cuerpoRenombrado = foldl (\acc (x, xf) -> sust acc x (Id xf)) cuerpo (zip xs frescos)
    -- sustituir simultáneamente los nombres nuevos por sus expresiones 's'
    in foldl (\acc (xf, s) -> sust acc xf s) cuerpoRenombrado (zip frescos ss)

-- Función auxiliar para generar variables frescas en masa
--refrescarNombres ["x", "y", "x"] ["x", "y"] -> ["x'", "y'", "x''"]
refrescarNombres :: [String] -> [String] -> [String]
refrescarNombres [] _ = []
refrescarNombres (x:xs) ocupados =
    let xf = freshName ocupados
    in xf : refrescarNombres xs (xf : ocupados)

------------------------------------
--OUUUUUUUUUUMAAAAAAAAAAAR-----------
------------------------------------
-- RETO 4: semantica operacional de paso grande (Omar Alejandro)
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.
bigStep :: ASA -> Maybe ASA
bigStep expression =
  case expression of
    Num n -> Just (Num n)
    Boolean b -> Just (Boolean b)
    Id _ -> Nothing
    And es -> evalNary es $ \vs -> Boolean . and <$> booleans vs
    Or es -> evalNary es $ \vs -> Boolean . or <$> booleans vs
    Add es -> evalNary es $ \vs -> Num . sum <$> naturals vs
    Sub es -> evalNary es $ \vs -> do
      ns <- naturals vs
      pure (Num (foldl truncatedSub (head ns) (tail ns)))
    Mul es -> evalNary es $ \vs -> Num . product <$> naturals vs
    Div es -> evalNary es $ \vs -> do
      ns <- naturals vs
      let divisors = tail ns
      if any (== 0) divisors
        then Nothing
        else Just (Num (foldl div (head ns) divisors))
    Lt es -> numericComparison (<) es
    Gt es -> numericComparison (>) es
    Le es -> numericComparison (<=) es
    Ge es -> numericComparison (>=) es
    Expt e1 e2 -> do
      Num n <- bigStep e1
      Num m <- bigStep e2
      if m < 0 then Nothing else Just (Num (n ^ m))
    EqP e1 e2 -> do
      v1 <- bigStep e1
      v2 <- bigStep e2
      equalValues v1 v2
    Not e -> do
      v <- bigStep e
      case v of
        Boolean b -> Just (Boolean (not b))
        Num _ -> Just (Boolean False)
        _ -> Nothing
    Add1 e -> do
      Num n <- bigStep e
      pure (Num (n + 1))
    Sub1 e -> do
      Num n <- bigStep e
      pure (Num (max 0 (n - 1)))
    ZeroP e -> do
      Num n <- bigStep e
      pure (Boolean (n == 0))
    Let bindings body
      | not (null bindings) && distinct (map fst bindings) -> do
          values <- traverse (bigStep . snd) bindings
          bigStep (sustMany body (zip (map fst bindings) values))
      | otherwise -> Nothing
    LetStar [] body -> bigStep body
    LetStar ((x, e) : bindings) body -> do
      value <- bigStep e
      bigStep (sust (LetStar bindings body) x value)
  where
    evalNary :: [ASA] -> ([ASA] -> Maybe ASA) -> Maybe ASA
    evalNary es delta
      | length es < 2 = Nothing
      | otherwise = traverse bigStep es >>= delta

    booleans :: [ASA] -> Maybe [Bool]
    booleans = traverse getBoolean
      where
        getBoolean (Boolean b) = Just b
        getBoolean _ = Nothing

    naturals :: [ASA] -> Maybe [Int]
    naturals = traverse getNatural
      where
        getNatural (Num n) = Just n
        getNatural _ = Nothing

    truncatedSub :: Int -> Int -> Int
    truncatedSub n m = max 0 (n - m)

    numericComparison :: (Int -> Int -> Bool) -> [ASA] -> Maybe ASA
    numericComparison comparison es = evalNary es $ \vs -> do
      ns <- naturals vs
      pure (Boolean (and (zipWith comparison ns (tail ns))))

    equalValues :: ASA -> ASA -> Maybe ASA
    equalValues (Num n) (Num m) = Just (Boolean (n == m))
    equalValues (Boolean b) (Boolean c) = Just (Boolean (b == c))
    equalValues _ _ = Nothing

    distinct :: Eq a => [a] -> Bool
    distinct [] = True
    distinct (x : xs) = x `notElem` xs && distinct xs
