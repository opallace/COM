module ModuleComando where

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

import ModuleDataTypes
import ModuleLexico
import ModuleExprL
import ModuleExpr

bloco = do{
                b <- braces (many comando);
                return (b)
              }
        <?> "expression"

comando =   do{
                reserved "if";
                e  <- parens exprL;
                b1 <- bloco;
                reserved "else";
                b2 <- bloco;
                return(If e b1 b2)
              }
        <|> do{
                reserved "while";
                e <- parens exprL;
                b <- bloco;
                return(While e b)
              }
        <|> do{
                reserved "read";
                i <- parens identifier;
                reserved ";";
                return (Leitura i)
              }
        <|> do{
                reserved "print";
                i <- parens identifier;
                reserved ";";
                return (Leitura i)
              }
        <|> do{
                reserved "return";
                i <- optionMaybe (parens expr);
                reserved ";";
                return (Ret (i))
              }
        <|> try(do{
                i <- identifier;
                reserved "=";
                e <- expr;
                reserved ";";
                return(Atrib i e)
              })
        <|> do{
                i <- identifier;
                p <- parens (commaSep expr);
                reserved ";";
                return (Proc i p)
              }
        <?> "expression"