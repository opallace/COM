module ModuleExpr where


import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

import ModuleDataTypes
import ModuleLexico

tabela   = [[prefix "-" Neg]
            , [binario "*" (:*:) AssocLeft, binario "/" (:/:) AssocLeft ]
            , [binario "+" (:+:) AssocLeft, binario "-" (:-:) AssocLeft ]
           ]

binario  name fun = Infix (do{reservedOp name; return fun })
prefix   name fun = Prefix (do{reservedOp name; return fun })
   
expr = buildExpressionParser tabela fator
       <?> "expression"   
        
fator = parens expr
       <|> do {
                 n <- number; 
                 case n of
                     Left natural -> return (Const (CInt natural))
                     Right float  -> return (Const (CDouble float))
              }
       <|> do{
              i <- stringLiteral;
              return (Lit i)
              }
       <|> try(do {
              i <- identifier;
              b <- parens (commaSep expr);
              return (Chamada i b)
              })
       <|> do{
              i <- identifier;
              return (IdVar i)
              }
       <?> "expression"
