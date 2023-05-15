import Text.Parsec

type Id       = String

data Type     = TDouble         
              | TInt            
              | TString       
              | TVoid 
                deriving Show 

data TCons    = CDouble Double  
              | CInt Int        
              | CString String        
                deriving Show

data Expr     = Expr :+: Expr   
              | Expr :-: Expr   
              | Expr :*: Expr 
              | Expr :/: Expr 
              | Neg Expr       
              | Const TCons 
              | IdVar String 
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
              | Ret Expr
              deriving Show


partida :: Parsec String u Programa
partida = do {e <- programa; eof; return e}

parserE = runParser partida [] "Programa"

parserExpr s = case parserE s of
                     Left er -> print er
                     Right v -> print v

main = do putStr "Arquivo: "
          f <- getLine
          e <- readFile f
          parserExpr e