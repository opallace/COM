module ModuleBloco where

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

import ModuleDataTypes
import ModuleComando

bloco =   do{
                b <- braces (many comando);
                return (b)
              }
        <?> "expression"