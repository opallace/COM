import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

import ModuleDataTypes
import ModulePrograma

partida :: Parsec String u Programa
partida = do {e <- programa; eof; return e}

parserE = runParser partida [] "Programa"

parserExpr s = case parserE s of
                     Left er -> print er
                     Right v -> print v

main = do 
          e <- readFile "teste.txt"
          putStr "Arquivo: "
          parserExpr e
