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
            b <- many comando;
            return (b)
        }
        <?> "expression"

comando =   do{
                reserved "if";
                e  <- parens exprL;
                b1 <- braces bloco;
                reserved "else";
                b2 <- braces bloco;
                return(If e b1 b2)
              }
        <|>do{
                reserved "if";
                e  <- parens exprL;
                b1 <- braces bloco;
                return(If e b1 [])
              }
        <|> do{
                reserved "while";
                e <- parens exprL;
                b <- braces bloco;
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
                i <- parens expr;
                reserved ";";
                return (Imp i)
              }
        <|> do{
                reserved "return";
                i <- optionMaybe expr;
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
