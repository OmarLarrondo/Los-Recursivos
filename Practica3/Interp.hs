module Interp where

import Grammars
import Data.List (nub)

-- Implementacion del reto 3: Ismael.
-- RETO 3: sustitucion nominal que evita captura
freeVars :: ASA -> [String]
freeVars = nub . go
  where
    go (Id x) = [x]
    go (Num _) = []
    go (Boolean _) = []
    go (And es) = concatMap go es
    go (Or es) = concatMap go es
    go (Add es) = concatMap go es
    go (Sub es) = concatMap go es
    go (Mul es) = concatMap go es
    go (Div es) = concatMap go es
    go (Lt es) = concatMap go es
    go (Gt es) = concatMap go es
    go (Le es) = concatMap go es
    go (Ge es) = concatMap go es
    go (Expt e1 e2) = go e1 ++ go e2
    go (EqP e1 e2) = go e1 ++ go e2
    go (Not e) = go e
    go (Add1 e) = go e
    go (Sub1 e) = go e
    go (ZeroP e) = go e
    go (Let bindings body) =
      concatMap (go . snd) bindings
        ++ filter (`notElem` map fst bindings) (go body)
    go (LetStar [] body) = go body
    go (LetStar ((x, e) : bindings) body) =
      go e ++ filter (/= x) (go (LetStar bindings body))

--agarrar todos los names del arbol, vars libres, ligadas y los names
--declarados en los bingings
names :: ASA -> [String]
names = nub . go
  where
    go (Id x) = [x]
    go (Num _) = []
    go (Boolean _) = []
    go (And es) = concatMap go es
    go (Or es) = concatMap go es
    go (Add es) = concatMap go es
    go (Sub es) = concatMap go es
    go (Mul es) = concatMap go es
    go (Div es) = concatMap go es
    go (Lt es) = concatMap go es
    go (Gt es) = concatMap go es
    go (Le es) = concatMap go es
    go (Ge es) = concatMap go es
    go (Expt e1 e2) = go e1 ++ go e2
    go (EqP e1 e2) = go e1 ++ go e2
    go (Not e) = go e
    go (Add1 e) = go e
    go (Sub1 e) = go e
    go (ZeroP e) = go e
    go (Let bindings body) = bindingNames bindings ++ go body
    go (LetStar bindings body) = bindingNames bindings ++ go body

    bindingNames = concatMap (\(x, e) -> x : go e)

--[z1,z2,...] infinitamente hasta que sea el bueno
freshName :: [String] -> String
freshName used = head (filter (`notElem` used) candidates)
  where
    candidates = "z" : ["z" ++ show n | n <- [(1 :: Int) ..]]


sust :: ASA -> String -> ASA -> ASA
sust expression x replacement =
  case expression of
    Id y | y == x -> replacement
         | otherwise -> Id y
    Num n -> Num n
    Boolean b -> Boolean b
    And es -> And (mapSubst es)
    Or es -> Or (mapSubst es)
    Add es -> Add (mapSubst es)
    Sub es -> Sub (mapSubst es)
    Mul es -> Mul (mapSubst es)
    Div es -> Div (mapSubst es)
    Lt es -> Lt (mapSubst es)
    Gt es -> Gt (mapSubst es)
    Le es -> Le (mapSubst es)
    Ge es -> Ge (mapSubst es)
    Expt e1 e2 -> Expt (recur e1) (recur e2)
    EqP e1 e2 -> EqP (recur e1) (recur e2)
    Not e -> Not (recur e)
    Add1 e -> Add1 (recur e)
    Sub1 e -> Sub1 (recur e)
    ZeroP e -> ZeroP (recur e)
    Let bindings body -> substLet bindings body
    LetStar [] body -> LetStar [] (recur body)
    LetStar ((y, e) : bindings) body -> substLetStar y e bindings body
  where
    recur e = sust e x replacement
    mapSubst = map recur

    substLet bindings body =
      let bindings' = map (\(y, e) -> (y, recur e)) bindings
          binders = map fst bindings
       in if x `elem` binders || x `notElem` freeVars body
            then Let bindings' body
            else
              let (renamedBinders, renamedBody) =
                    renameLetBinders
                      binders
                      body
                      (names body ++ binders ++ names replacement ++ [x])
               in Let
                    (zip renamedBinders (map snd bindings'))
                    (recur renamedBody)

    renameLetBinders [] body _ = ([], body)
    renameLetBinders (y : ys) body used
      | y `elem` freeVars replacement =
          let fresh = freshName used
              body' = sust body y (Id fresh)
              (ys', finalBody) = renameLetBinders ys body' (fresh : used)
           in (fresh : ys', finalBody)
      | otherwise =
          let (ys', finalBody) = renameLetBinders ys body used
           in (y : ys', finalBody)

    substLetStar y e bindings body =
      let e' = recur e
          scope = LetStar bindings body
       in if y == x || x `notElem` freeVars scope
            then LetStar ((y, e') : bindings) body
            else
              if y `elem` freeVars replacement
                then
                  let fresh = freshName (names scope ++ names replacement ++ [x, y])
                      renamedScope = sust scope y (Id fresh)
                   in case recur renamedScope of
                        LetStar bindings' body' ->
                          LetStar ((fresh, e') : bindings') body'
                        _ -> error "Invariante interna rota al sustituir let*"
                else
                  case recur scope of
                    LetStar bindings' body' ->
                      LetStar ((y, e') : bindings') body'
                    _ -> error "Invariante interna rota al sustituir let*"


sustMany :: ASA -> [Binding] -> ASA
sustMany body bindings =
  let used = names body ++ concatMap (names . snd) bindings ++ map fst bindings
      placeholders = takeFresh (length bindings) used
      renamedBody =
        foldl
          (\current ((x, _), fresh) -> sust current x (Id fresh))
          body
          (zip bindings placeholders)
   in foldl
        (\current (fresh, (_, value)) -> sust current fresh value)
        renamedBody
        (zip placeholders bindings)
  where
    takeFresh 0 _ = []
    takeFresh n used =
      let fresh = freshName used
       in fresh : takeFresh (n - 1) (fresh : used)

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
