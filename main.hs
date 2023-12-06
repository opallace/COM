import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

import ModuleDataTypes
import ModulePrograma
import ModuleSemantic
import GenBytecode

partida :: Parsec String u Programa
partida = do {e <- programa; eof; return e}

parserE   = runParser partida [] "Programa"

parserExpr s = case parserE s of
                    Left er -> print er
                    Right s -> case verProg s of
                                    MS(erro, prog) -> do {
                                                            putStr erro;
                                                            writeFile "Wallace.k7" (gerar "Wallace" prog);
                                                        }

main = do 
          e <- readFile "entrada.k7"
          parserExpr e
