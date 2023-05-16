module ModulePrograma where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language

import ModuleDataTypes
import ModuleLexico
import ModuleFuncao

programa = do{
                t <- parens (comma comma);
                return (Prog t)
             }