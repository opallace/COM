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

verificaExpr (Const (CInt x))    = pure(Const (CInt x), TInt)
verificaExpr (Const (CDouble x)) = pure(Const (CDouble x), TDouble)
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

verificaComandos (If e bl1 bl2) =
verificaComandos (While el bl) = 
verificaComandos (DoWhile el bl) = 
verificaComandos (Atrib id e) = 
verificaComandos (Leitura id) = 
verificaComandos (Imp e) =
verificaComandos (Ret (Maybe e)) =
verificaComandos (Proc id exprs) =   

verificaBloco cabecalho_funcoes vars comandos =
    do if verificaVariaveisDuplicadas vars then erro("Variaveis duplicadas\n")(vars,comandos) 
        else 
            cmd <- verificaComandos comandos
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
