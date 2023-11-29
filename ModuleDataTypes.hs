module ModuleDataTypes where

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

type Id       = String

data Type     = TDouble         
              | TInt            
              | TString       
              | TVoid 
                deriving (Show, Eq)

data TCons    = CDouble Double  
              | CInt Integer           
                deriving Show

data Expr     = Expr :+: Expr   
              | Expr :-: Expr   
              | Expr :*: Expr 
              | Expr :/: Expr 
              | Neg Expr       
              | Const TCons 
              | IdVar Id 
              | Chamada Id [Expr] 
              | Lit String 
              | IntDouble Expr 
              | DoubleInt Expr
                deriving Show

data ExprR    = Expr :==: Expr  
              | Expr :/=: Expr  
              | Expr :<: Expr 
              | Expr :>: Expr 
              | Expr :<=: Expr 
              | Expr :>=: Expr 
                deriving Show

data ExprL    = ExprL :&: ExprL 
              | ExprL :|: ExprL 
              | Not ExprL 
              | Rel ExprR 
                deriving Show

data Var      = Id :#: (Type, Int)                                   
                deriving Show

data Funcao   = Id :->: ([Var], Type)                          
                deriving Show

data Programa = Prog [Funcao] [(Id,[Var],Bloco)] [Var] Bloco 
                deriving Show

type Bloco    = [Comando]

data Comando  = If ExprL Bloco Bloco
              | While ExprL Bloco
              | DoWhile ExprL Bloco
              | Atrib Id Expr
              | Leitura Id
              | Imp Expr
              | Ret (Maybe Expr)
              | Proc Id [Expr]
              deriving Show

