module Interp where

import Grammars

-- RETO 3: sustitucion nominal que evita captura
freeVars :: ASA -> [String]

names :: ASA -> [String]

freshName :: [String] -> String

sust :: ASA -> String -> ASA -> ASA

sustMany :: ASA -> [Binding] -> ASA

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
