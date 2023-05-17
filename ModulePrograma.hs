module ModulePrograma where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language

import ModuleDataTypes
import ModuleLexico
import ModuleFuncao
import ModuleVar

programa = do {
                 fn <- many funcao;
                 main <- braces bloco_var;
                 return(Prog (map fst fn) (map snd fn) (fst main) (snd main))
              }
             