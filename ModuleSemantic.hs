module ModuleSemantic where

import ModuleDataTypes
import ModuleLexico
import ModuleFuncao
import ModuleVar
import Data.Functor.Classes (eq1)
import Foreign.C (e2BIG)

data M a = MS(String, a) deriving Show

erro s a    = MS("Erro: "++s, a)
warning s a = MS("Warning: "++s, a)

instance Functor M where
    fmap f (MS (s, a)) = MS(s, f a)

instance Applicative M where
    pure x = MS("", x)
    MS(s1, f) <*> MS(s2, x) = MS(s1 <> s2, f x) 

instance Monad M where
    MS m >>= f = let(s, a) = m in let MS(s', b) = f a in MS (s++s', b)

-- passar vars para expr para verificar existencia e tipo de variaveis em expr
verificaExpr [] (IdVar id) = erro("Variavel não encontrada\n")(TVoid, IdVar id)
verificaExpr (i:#:t):vars (IdVar id) 
    | i == id then pure(t, IdVar id)
    | otherwise =  verificaExpr vars (IdVar id)
verificaExpr (Const (CInt x))    = pure(Const (CInt x), TInt)
verificaExpr (Const (CDouble x)) = pure(Const (CDouble x), TDouble)
verificaExpr (Neg e) =
    do  (e', t) <- verificaExpr e
        pure(Neg e', t)
verificaExpr (e1:+:e2) = 
    do  (e1', t1) <- verificaExpr e1
        (e2', t2) <- verificaExpr e2
          
        case (t1, t2) of
            (TInt, TInt)       -> pure(e1' :+: e2', TInt)
            (TDouble, TInt)    -> pure(e1' :+: e2', TDouble)
            (TInt, TDouble)    -> pure(e1' :+: e2', TDouble)
            (TDouble, TDouble) -> pure(e1' :+: e2', TDouble)
            (TString, _)       -> erro("Tipo String não compativel com a operação\n")(e1' :+: e2', TString)
            (_, TString)       -> erro("Tipo String não compativel com a operação\n")(e1' :+: e2', TString)
            (_, TVoid)         -> erro("Tipo Void não compativel com a operação\n")(e1' :+: e2', TVoid)
            (TVoid, _)         -> erro("Tipo Void não compativel com a operação\n")(e1' :+: e2', TVoid)
verificaExpr (e1:-:e2) = 
    do  (e1', t1) <- verificaExpr e1
        (e2', t2) <- verificaExpr e2
          
        case (t1, t2) of
            (TInt, TInt) -> pure(e1' :-: e2', TInt)
            (TDouble, TInt) -> pure(e1' :-: e2', TDouble)
            (TInt, TDouble) -> pure(e1' :-: e2', TDouble)
            (TDouble, TDouble) -> pure(e1' :-: e2', TDouble)
            (TString, _)       -> erro("Tipo String não compativel com a operação\n")(e1' :-: e2', TString)
            (_, TString)       -> erro("Tipo String não compativel com a operação\n")(e1' :-: e2', TString)
            (_, TVoid)         -> erro("Tipo Void não compativel com a operação\n")(e1' :-: e2', TVoid)
            (TVoid, _)         -> erro("Tipo Void não compativel com a operação\n")(e1' :-: e2', TVoid)
verificaExpr (e1:*:e2) = 
    do  (e1', t1) <- verificaExpr e1
        (e2', t2) <- verificaExpr e2
          
        case (t1, t2) of
            (TInt, TInt) -> pure(e1' :*: e2', TInt)
            (TDouble, TInt) -> pure(e1' :*: e2', TDouble)
            (TInt, TDouble) -> pure(e1' :*: e2', TDouble)
            (TDouble, TDouble) -> pure(e1' :*: e2', TDouble)
            (TString, _)       -> erro("Tipo String não compativel com a operação\n")(e1' :*: e2', TString)
            (_, TString)       -> erro("Tipo String não compativel com a operação\n")(e1' :*: e2', TString)
            (_, TVoid)         -> erro("Tipo Void não compativel com a operação\n")(e1' :*: e2', TVoid)
            (TVoid, _)         -> erro("Tipo Void não compativel com a operação\n")(e1' :*: e2', TVoid)
verificaExpr (e1:/:e2) = 
    do  (e1', t1) <- verificaExpr e1
        (e2', t2) <- verificaExpr e2
          
        case (t1, t2) of
            (TInt, TInt) -> pure(e1' :/: e2', TInt)
            (TDouble, TInt) -> pure(e1' :/: e2', TDouble)
            (TInt, TDouble) -> pure(e1' :/: e2', TDouble)
            (TDouble, TDouble) -> pure(e1' :/: e2', TDouble)
            (TString, _)       -> erro("Tipo String não compativel com a operação\n")(e1' :/: e2', TString)
            (_, TString)       -> erro("Tipo String não compativel com a operação\n")(e1' :/: e2', TString)
            (_, TVoid)         -> erro("Tipo Void não compativel com a operação\n")(e1' :/: e2', TVoid)
            (TVoid, _)         -> erro("Tipo Void não compativel com a operação\n")(e1' :/: e2', TVoid)

verificaExprR (e1:==:e2) =
    do  (e1', t1) <- verificaExpr e1
        (e2', t2) <- verificaExpr e2

        case (t1, t2) of
                (TInt, TInt) -> pure(e1' :==: e2', TInt)
                (TDouble, TInt) -> pure(e1' :==: e2', TDouble)
                (TInt, TDouble) -> pure(e1' :==: e2', TDouble)
                (TDouble, TDouble) -> pure(e1' :==: e2', TDouble)
                (TString, _)       -> erro("Tipo String não compativel com a operação\n")(e1' :==: e2', TString)
                (_, TString)       -> erro("Tipo String não compativel com a operação\n")(e1' :==: e2', TString)
                (_, TVoid)         -> erro("Tipo Void não compativel com a operação\n")(e1' :==: e2', TVoid)
                (TVoid, _)         -> erro("Tipo Void não compativel com a operação\n")(e1' :==: e2', TVoid)
verificaExprR (e1:/=:e2) =
    do  (e1', t1) <- verificaExpr e1
        (e2', t2) <- verificaExpr e2

        case (t1, t2) of
                (TInt, TInt) -> pure(e1' :/=: e2', TInt)
                (TDouble, TInt) -> pure(e1' :/=: e2', TDouble)
                (TInt, TDouble) -> pure(e1' :/=: e2', TDouble)
                (TDouble, TDouble) -> pure(e1' :/=: e2', TDouble)
                (TString, _)       -> erro("Tipo String não compativel com a operação\n")(e1' :/=: e2', TString)
                (_, TString)       -> erro("Tipo String não compativel com a operação\n")(e1' :/=: e2', TString)
                (_, TVoid)         -> erro("Tipo Void não compativel com a operação\n")(e1' :/=: e2', TVoid)
                (TVoid, _)         -> erro("Tipo Void não compativel com a operação\n")(e1' :/=: e2', TVoid)
verificaExprR (e1:<:e2) =
    do  (e1', t1) <- verificaExpr e1
        (e2', t2) <- verificaExpr e2

        case (t1, t2) of
                (TInt, TInt) -> pure(e1' :<: e2', TInt)
                (TDouble, TInt) -> pure(e1' :<: e2', TDouble)
                (TInt, TDouble) -> pure(e1' :<: e2', TDouble)
                (TDouble, TDouble) -> pure(e1' :<: e2', TDouble)
                (TString, _)       -> erro("Tipo String não compativel com a operação\n")(e1' :<: e2', TString)
                (_, TString)       -> erro("Tipo String não compativel com a operação\n")(e1' :<: e2', TString)
                (_, TVoid)         -> erro("Tipo Void não compativel com a operação\n")(e1' :<: e2', TVoid)
                (TVoid, _)         -> erro("Tipo Void não compativel com a operação\n")(e1' :<: e2', TVoid)
verificaExprR (e1:>:e2) =
    do  (e1', t1) <- verificaExpr e1
        (e2', t2) <- verificaExpr e2

        case (t1, t2) of
                (TInt, TInt) -> pure(e1' :>: e2', TInt)
                (TDouble, TInt) -> pure(e1' :>: e2', TDouble)
                (TInt, TDouble) -> pure(e1' :>: e2', TDouble)
                (TDouble, TDouble) -> pure(e1' :>: e2', TDouble)
                (TString, _)       -> erro("Tipo String não compativel com a operação\n")(e1' :>: e2', TString)
                (_, TString)       -> erro("Tipo String não compativel com a operação\n")(e1' :>: e2', TString)
                (_, TVoid)         -> erro("Tipo Void não compativel com a operação\n")(e1' :>: e2', TVoid)
                (TVoid, _)         -> erro("Tipo Void não compativel com a operação\n")(e1' :>: e2', TVoid)
verificaExprR (e1:<=:e2) =
    do  (e1', t1) <- verificaExpr e1
        (e2', t2) <- verificaExpr e2

        case (t1, t2) of
                (TInt, TInt) -> pure(e1' :<=: e2', TInt)
                (TDouble, TInt) -> pure(e1' :<=: e2', TDouble)
                (TInt, TDouble) -> pure(e1' :<=: e2', TDouble)
                (TDouble, TDouble) -> pure(e1' :<=: e2', TDouble)
                (TString, _)       -> erro("Tipo String não compativel com a operação\n")(e1' :<=: e2', TString)
                (_, TString)       -> erro("Tipo String não compativel com a operação\n")(e1' :<=: e2', TString)
                (_, TVoid)         -> erro("Tipo Void não compativel com a operação\n")(e1' :<=: e2', TVoid)
                (TVoid, _)         -> erro("Tipo Void não compativel com a operação\n")(e1' :<=: e2', TVoid)
verificaExprR (e1:>=:e2) =
    do  (e1', t1) <- verificaExpr e1
        (e2', t2) <- verificaExpr e2

        case (t1, t2) of
                (TInt, TInt) -> pure(e1' :>=: e2', TInt)
                (TDouble, TInt) -> pure(e1' :>=: e2', TDouble)
                (TInt, TDouble) -> pure(e1' :>=: e2', TDouble)
                (TDouble, TDouble) -> pure(e1' :>=: e2', TDouble)
                (TString, _)       -> erro("Tipo String não compativel com a operação\n")(e1' :>=: e2', TString)
                (_, TString)       -> erro("Tipo String não compativel com a operação\n")(e1' :>=: e2', TString)
                (_, TVoid)         -> erro("Tipo Void não compativel com a operação\n")(e1' :>=: e2', TVoid)
                (TVoid, _)         -> erro("Tipo Void não compativel com a operação\n")(e1' :>=: e2', TVoid)

verificaExprL (el1:&:el2) =
    do  el1' <- verificaExprL el1
        el2' <- verificaExprL el2
        pure(el1' :&: el2')
verificaExprL (el1:|:el2) =
    do  el1' <- verificaExprL el1
        el2' <- verificaExprL el2
        pure(el1' :|: el2')
verificaExprL (Not el) =
    do  el' <- verificaExprL el
        pure(Not el')
verificaExprL (Rel er) =
    do  (er', t) <- verificaExprR er
        pure(Rel er')

verificaComandos cabecalho_funcoes vars (If el bl1 bl2) = 
    do  el' <- verificaExprL el
        bl1' <- mapM (verificaComandos cabecalho_funcoes vars) bl1
        bl2' <- mapM (verificaComandos cabecalho_funcoes vars) bl2
        pure(If el' bl1' bl2')
verificaComandos cabecalho_funcoes vars (While el bl) = 
    do  el' <- verificaExprL el
        bl' <- mapM (verificaComandos cabecalho_funcoes vars) bl
        pure(While el' bl')
verificaComandos cabecalho_funcoes vars (DoWhile el bl) = 
    do  el' <- verificaExprL el
        bl' <- mapM (verificaComandos cabecalho_funcoes vars) bl
        pure(DoWhile el' bl')
verificaComandos cabecalho_funcoes vars (Atrib id e) = 
    do  (e1, t1) <- verificaExpr (IdVar id)
        (e2, t2) <- verificaExpr e

        case (t1, t2) of
            (TInt,TInt) -> pure (Atrib id e2)
            (TDouble, TDouble) -> pure (Atrib id e2)
            (TString,TString) -> pure (Atrib id e2)
            

-- verificaComandos cabecalho_funcoes (Leitura id) = 
-- verificaComandos cabecalho_funcoes (Imp e) =
-- verificaComandos cabecalho_funcoes (Ret (Maybe e)) =
-- verificaComandos cabecalho_funcoes (Proc id exprs) =   

verificaBloco cabecalho_funcoes vars comandos =
    do if verificaVariaveisDuplicadas vars then erro("Variaveis duplicadas\n")(vars,comandos) 
        else 
            do  cmd <- mapM (verificaComandos cabecalho_funcoes vars) comandos 
                pure(vars, cmd)

verificaBlocoFuncao cabecalho_funcoes [] = pure([])
verificaBlocoFuncao cabecalho_funcoes ((id, vars, bloco):funcoes) = 
    do  (v,b) <- verificaBloco cabecalho_funcoes vars bloco
        r <- verificaBlocoFuncao cabecalho_funcoes funcoes
        pure((id,v,b):r)

existeVar a [] = False
existeVar a ((id :#: t):vars) = if a == id then True else existeVar a vars

verificaVariaveisDuplicadas [] = False
verificaVariaveisDuplicadas ((id :#: t):vars) = if existeVar id vars then True else verificaVariaveisDuplicadas vars

existeFuncao a [] = False
existeFuncao a ((id :->: ((var:vs),t)):fs) = if a == id then True else existeFuncao a fs

verificaFuncoesDuplicadas [] = False
verificaFuncoesDuplicadas ((id :->: ((var:vs),t)):fs) = if existeFuncao id fs then True else verificaFuncoesDuplicadas fs


verificaPrograma (Prog cabecalho_funcoes funcoes var_principal bloco_principal) = 
    do  if verificaFuncoesDuplicadas cabecalho_funcoes then erro("Funções duplicadas\n")(Prog cabecalho_funcoes funcoes var_principal bloco_principal) 
        else pure(Prog cabecalho_funcoes funcoes var_principal bloco_principal)

        f <- verificaBlocoFuncao cabecalho_funcoes funcoes

        (vars, bloco) <- verificaBloco cabecalho_funcoes var_principal bloco_principal
        pure(Prog cabecalho_funcoes f vars bloco)
