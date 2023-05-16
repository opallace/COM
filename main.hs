import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

import ModuleDataTypes
import ModuleExpr
import ModuleExprL
import ModuleExprR
import ModuleVar
import ModuleFuncao

partida :: Parsec String u Funcao
partida = do {e <- funcao; eof; return e}

parserE = runParser partida [] "Programa"

parserExpr s = case parserE s of
                     Left er -> print er
                     Right v -> print v

main = do putStr "Arquivo: "
          e <- readFile "arquivo.txt"
          parserExpr e
