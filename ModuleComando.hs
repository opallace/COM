module ModuleComando where

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

import ModuleDataTypes
import ModuleLexico
import ModuleExprL
import ModuleBloco
import ModuleExpr

comando =   do{
                reserved "if";
                e <- exprL;
                b1 <- bloco;
                b2 <- bloco;
                return(If e b1 b2)
              }
        <|> do{
                reserved "While";
                e <- exprL;
                b <- bloco;
                return(While e b)
              }
        <|> do{
                i <- identifier;
                reserved "=";
                e <- expr;
                return(Atrib i e)
              }
        <|> do{
                reserved "read";
                i <- parens identifier;
                return (Leitura i)
              }
        <?> "expression"