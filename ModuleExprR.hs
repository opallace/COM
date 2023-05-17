module ModuleExprR where


import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language

import ModuleDataTypes
import ModuleLexico
import ModuleExpr


exprR = parens exprR
       <|> do {
                e1 <- expr;
                op <- operation;
                e2 <- expr;
                return (op e1 e2)
              }
       <?> "expression"

operation =    do {reservedOp "=="; return (:==:)}
           <|> do {reservedOp "/="; return (:/=:)}
           <|> do {reservedOp "<";  return (:<:)}
           <|> do {reservedOp ">";  return (:>:)}
           <|> do {reservedOp "<="; return (:<=:)}
           <|> do {reservedOp ">="; return (:>=:)}
