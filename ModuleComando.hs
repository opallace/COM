module ModuleComando where

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

import ModuleDataTypes
import ModuleLexico
import ModuleExprL
import ModuleExpr

atrib_for = do {
                i <- identifier;
                reserved "=";
                e <- expr;
                return(Atrib i e)
               }
               

params_for = do {
                    a1 <- atrib_for;
                    reserved ";";
                    el <- exprL;
                    reserved ";";
                    a2 <- atrib_for;

                    return(a1, (el, a2))
                }

bloco = do{
            b <- many comando;
            return (concat b)
        }

comando =   do{
                reserved "for";
                (a1, (el, a2))  <- parens params_for;
                b <- braces bloco;
                return([a1,While el (b++[a2])])
              }
        <|> do{
                reserved "if";
                e  <- parens exprL;
                b1 <- braces bloco;
                reserved "else";
                b2 <- braces bloco;
                return[(If e b1 b2)]
              }
        <|>do{
                reserved "if";
                e  <- parens exprL;
                b1 <- braces bloco;
                return[(If e b1 [])]
              }
        <|> do{
                reserved "do";
                b <- braces bloco;
                reserved "while";
                e <- parens exprL;
                reserved ";";
                return[(DoWhile e b)]
              }
        <|> do{
                reserved "while";
                e <- parens exprL;
                b <- braces bloco;
                return[(While e b)]
              }
        <|> do{
                reserved "read";
                i <- parens identifier;
                reserved ";";
                return [(Leitura i)]
              }
        <|> do{
                reserved "print";
                i <- parens expr;
                reserved ";";
                return [(Imp i)]
              }
        <|> do{
                reserved "return";
                i <- optionMaybe expr;
                reserved ";";
                return [(Ret (i))]
              }
        <|> try(do{
                i <- identifier;
                reserved "=";
                e <- expr;
                reserved ";";
                return[(Atrib i e)]
              })

        <|> try(do{
                  i <- identifier;
                  reserved "++";
                  reserved ";";
                  return[(Atrib i (IdVar i :+: Const (CInt 1)))]

               })
        <|> do{
                i <- identifier;
                p <- parens (commaSep expr);
                reserved ";";
                return [(Proc i p)]
              }
        <?> "expression"
