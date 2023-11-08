module ModuleSemantic where

import ModuleDataTypes
import ModuleLexico
import ModuleFuncao
import ModuleVar
import Data.Functor.Classes (eq1)
import Foreign.C (e2BIG)

data M a = MS(String, a) deriving Show

erro s    = MS("Erro: "++s, ())
warning s = MS("Warning: "++s, ())

instance Functor M where
    fmap f (MS (s, a)) = MS(s, f a)

instance Applicative M where
    pure x = MS("", x)
    MS(s1, f) <*> MS(s2, x) = MS(s1 <> s2, f x) 

instance Monad M where
--    return x = MS("", x)
    MS m >>= f = let(s, a) = m in let MS(s', b) = f a in MS (s++s', b)


verificaExpr o e1  = do (e1', t1) <- verificaExpr e1
                              (e2', t2) <- verificaExpr e2
                              
                              case (t1, t2) of
                                (TInt, TInt) -> pure (e1' ++ e2')
    



verificaPrograma prog = do return ("teste", TInt)