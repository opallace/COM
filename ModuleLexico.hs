module ModuleLexico where


import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

import ModuleDataTypes

lingDef = emptyDef
          {   T.commentStart    = "{-"
            , T.commentEnd      = "-}"
            , T.commentLine     = "--"
            , T.reservedOpNames = ["+", "-", "/", "*", "<", ">", "<=", ">=", "==", "/=", "&&", "||", "!"]
            , T.reservedNames   = ["return", "if", "while", "=", "print", "read"]
            , T.identStart      = letter <|> char '_'
            , T.identLetter     = alphaNum <|> char '_' 
          }

lexico = T.makeTokenParser lingDef

number        = T.naturalOrFloat lexico
symbol        = T.symbol lexico
parens        = T.parens lexico
reservedOp    = T.reservedOp lexico
identifier    = T.identifier lexico
stringLiteral = T.stringLiteral lexico
commaSep      = T.commaSep lexico
reserved      = T.reserved lexico
braces        = T.braces lexico