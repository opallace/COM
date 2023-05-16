module ModuleExprL where


import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

import ModuleDataTypes
import ModuleLexico

tabelaL   = [[prefix "-" Neg]
            , [binario "*" (:*:) AssocLeft, binario "/" (:/:) AssocLeft ]
            , [binario "+" (:+:) AssocLeft, binario "-" (:-:) AssocLeft ]
           ]

binario  name fun = Infix (do{reservedOp name; return fun })
prefix   name fun = Prefix (do{reservedOp name; return fun })
   
exprL = buildExpressionParser tabelaL fator
       <?> "expression"   
        
fatorL = parens exprL
       <|> do {