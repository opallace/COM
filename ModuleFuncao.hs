module ModuleFuncao where

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

import ModuleDataTypes
import ModuleLexico
import ModuleVar
import ModuleComando

funcao = do
               t <- tipo
               i <- identifier
               a <- parens (commaSep declaracao_parametros)
               blk <- braces bloco_var
               let ret = (i, fst blk, snd blk)
               return (i :->: (a,t), ret) 
            