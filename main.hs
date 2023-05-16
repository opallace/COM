import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

import ModuleDataTypes
import ModuleExpr

partida :: Parsec String u Expr
partida = do {e <- expr; eof; return e}

parserE = runParser partida [] "Programa"

parserExpr s = case parserE s of
                     Left er -> print er
                     Right v -> print v

main = do putStr "Arquivo: "
          e <- readFile "arquivo.txt"
          parserExpr e
