module ModuleVar where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language

import ModuleDataTypes
import ModuleLexico


var = do {
                t <- tipo;
                i <- identifier;
                return (i :#: t)
              }
       <?> "expression"
       

tipo =      do {reserved "int"   ; return TInt}
        <|> do {reserved "double"; return TDouble}
        <|> do {reserved "string"; return TString}
        <|> do {reserved "void"  ; return TVoid}
