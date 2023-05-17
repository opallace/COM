module ModuleVar where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language

import ModuleDataTypes
import ModuleLexico
import ModuleComando

bloco_var = do{
                v <- many declaracao_var;
                b <- bloco;
                
                return (concat v, b)
            }

declaracao_var = do {
                t <- tipo;
                i <- commaSep identifier;
                reserved ";";
                return (map (:#: t) i)
              }
       <?> "expression"

declaracao_parametros = do {
                t <- tipo;
                i <- identifier;
                return (i :#: t)
              }
       <?> "expression"
       
tipo =      do {reserved "int"   ; return TInt}
        <|> do {reserved "double"; return TDouble}
        <|> do {reserved "string"; return TString}
        <|> do {reserved "void"  ; return TVoid}
