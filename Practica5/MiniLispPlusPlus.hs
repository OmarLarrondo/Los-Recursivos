module MiniLispPlusPlus where

import Control.Monad.IO.Class (liftIO)
import Grammars
import Interp
import Lexer
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)

-- RETO 5: integrar el combinador Y ----------------------------------------
-- [Autor: Omar]

-- Representa en el ASA del nucleo el combinador clasico:
--
-- Y = lambda f.
--       (lambda x. f (x x))
--       (lambda x. f (x x))
combinadorY :: ASA
combinadorY =
  Fun "f"
    (App
      (Fun "x" (App (Id "f") (App (Id "x") (Id "x"))))
      (Fun "x" (App (Id "f") (App (Id "x") (Id "x")))))

-- Evalua combinadorY en el ambiente vacio y asocia su valor con el nombre Y.
-- La evaluacion se hace una sola vez y queda compartida en el ambiente
-- inicial. [Omar]
prelude :: Env
prelude =
  let valorY = sinFallo (bigStep [] combinadorY)
      sinFallo (Just v) = v
      sinFallo Nothing = error "El combinador Y no evalua a un valor"
   in [("Y", valorY)]

-- Evalua un programa del nucleo desde prelude y exige el resultado final.
-- [Omar]
evaluaNucleo :: ASA -> Maybe Value
evaluaNucleo programa = thenMaybe (bigStep prelude programa) strict

-- Integra el analisis, el desazucarado y la evaluacion desde prelude.
-- El resultado final debe pasar por strict antes de devolverse. [Omar]
evalua :: String -> Maybe Value
evalua entrada =
  thenMaybe (desugar (parse (lexer entrada))) evaluaNucleo

-- Infraestructura provista. No forma parte de los retos.
repl :: IO ()
repl = runInputT defaultSettings loop

loop :: InputT IO ()
loop =
  getInputLine "MiniLisp++> " >>= maybe (pure ()) procesaEntrada

procesaEntrada :: String -> InputT IO ()
procesaEntrada ":q" = pure ()
procesaEntrada entrada =
  liftIO (maybe muestraBloqueo print (evalua entrada)) >> loop

muestraBloqueo :: IO ()
muestraBloqueo =
  putStrLn "Error: evaluacion bloqueada"

main :: IO ()
main = repl
