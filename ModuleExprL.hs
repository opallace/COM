module ModuleExprL where


import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

import ModuleDataTypes
import ModuleLexico
import ModuleExprR

tabelaL   = [
            [prefix "!" Not],
            [binario "&&" (:&:) AssocLeft],
            [binario "||" (:|:) AssocLeft]
           ]

binario  name fun = Infix (do{reservedOp name; return fun })
prefix   name fun = Prefix (do{reservedOp name; return fun })
   
exprL = buildExpressionParser tabelaL fatorL
       <?> "expression"   
        
fatorL = parens exprL
       <|> do {
                er <- exprR;
                return (Rel er)
              }
       <?> "expression"
