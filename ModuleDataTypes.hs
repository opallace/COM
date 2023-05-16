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
                deriving Show 

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

data Var      = Id :#: Type                                    
                deriving Show

data Funcao   = Id :->: (Id, [Var], Bloco)                          
                deriving Show

data Programa = Prog [Funcao] Bloco 
                deriving Show
                
type Bloco    = [Comando]

data Comando  = If ExprL Bloco Bloco
              | While ExprL Bloco
              | Atrib Id Expr
              | Leitura Id
              | Imp Expr
              | Ret (Maybe Expr)
              | Proc Id [Expr]
              deriving Show
