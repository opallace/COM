import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

import ModuleDataTypes
import ModulePrograma
import ModuleSemanticPrograma

partida :: Parsec String u Programa
partida = do {e <- programa; eof; return e}

parserE   = runParser partida [] "Programa"

parserExpr s = let (a, erro, prog) = verificaPrograma (parserE s)
               in print erro

main = do 
          e <- readFile "teste.txt"
          putStr "Arquivo: "
          parserExpr e
