module ModuleFuncao where

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

import ModuleDataTypes
import ModuleLexico
import ModuleVar

funcao = do{
               t <- tipo;
               i <- identifier;
               a <- parens (commaSep var);
               return (i :->: (a,t)) 
            }    
        <?> "expression"