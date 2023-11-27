module ModuleSemantic where

import ModuleDataTypes
import ModuleLexico
import ModuleFuncao
import ModuleVar
import Data.Functor.Classes (eq1)
import Foreign.C (e2BIG)

data M a = MS(String, a) deriving Show

erro s a    = MS ("Erro: "++s, a)
warning s a = MS ("Warning: "++s, a)

instance Functor M where
    fmap f (MS (s, a)) = MS (s, f a)

instance Applicative M where
    pure x = MS ("", x)
    MS(s1, f) <*> MS(s2, x) = MS (s1 <> s2, f x)

instance Monad M where
    MS m >>= f = let(s, a) = m in let MS(s', b) = f a in MS (s++s', b)

verificaExpr tfuns [] (IdVar id) = erro("Variavel "++id++" não encontrada.\n") (IdVar id, TVoid)
verificaExpr tfuns ((i:#:t):tvars) (IdVar id) =
    if i == id then pure (IdVar id, t)
    else verificaExpr tfuns tvars (IdVar id)
verificaExpr tfuns tvars (Const (CInt x))    = pure (Const (CInt x), TInt)
verificaExpr tfuns tvars (Const (CDouble x)) = pure (Const (CDouble x), TDouble)
verificaExpr tfuns tvars (Lit s)    = pure (Lit s , TString)
verificaExpr tfuns tvars (IntDouble x)       = pure (IntDouble x, TDouble)
verificaExpr tfuns tvars (DoubleInt x)       = pure (DoubleInt x, TDouble)
verificaExpr tfuns tvars (Neg e) =
    do  (e', t) <- verificaExpr tfuns tvars e
        pure (Neg e', t)
verificaExpr tfuns tvars (Chamada id exprs) =
    do  exprs' <- mapM (verificaExpr tfuns tvars) exprs
        (i, argumentos_esperados, t) <- recuperaFuncao tfuns id
        argumentos_verificados <- verificaArgumentos id argumentos_esperados exprs'
        pure (Chamada id argumentos_verificados, t)
verificaExpr tfuns tvars (e1:+:e2) =
    do  (e1', t1) <- verificaExpr tfuns tvars e1
        (e2', t2) <- verificaExpr tfuns tvars e2

        case (t1, t2) of
            (TInt, TInt)       -> pure (e1' :+: e2', TInt)
            (TDouble, TInt)    -> pure (e1' :+: e2', TDouble)
            (TInt, TDouble)    -> pure (e1' :+: e2', TDouble)
            (TDouble, TDouble) -> pure (e1' :+: e2', TDouble)
            (TString, _)       -> erro "Tipo String não compativel com a operação.\n" (e1' :+: e2', TString)
            (_, TString)       -> erro "Tipo String não compativel com a operação.\n" (e1' :+: e2', TString)
            (_, TVoid)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :+: e2', TVoid)
            (TVoid, _)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :+: e2', TVoid)
verificaExpr tfuns tvars (e1:-:e2) =
    do  (e1', t1) <- verificaExpr tfuns tvars e1
        (e2', t2) <- verificaExpr tfuns tvars e2

        case (t1, t2) of
            (TInt, TInt) -> pure (e1' :-: e2', TInt)
            (TDouble, TInt) -> pure (e1' :-: e2', TDouble)
            (TInt, TDouble) -> pure (e1' :-: e2', TDouble)
            (TDouble, TDouble) -> pure (e1' :-: e2', TDouble)
            (TString, _)       -> erro "Tipo String não compativel com a operação.\n" (e1' :-: e2', TString)
            (_, TString)       -> erro "Tipo String não compativel com a operação.\n" (e1' :-: e2', TString)
            (_, TVoid)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :-: e2', TVoid)
            (TVoid, _)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :-: e2', TVoid)
verificaExpr tfuns tvars (e1:*:e2) =
    do  (e1', t1) <- verificaExpr tfuns tvars e1
        (e2', t2) <- verificaExpr tfuns tvars e2

        case (t1, t2) of
            (TInt, TInt) -> pure (e1' :*: e2', TInt)
            (TDouble, TInt) -> pure (e1' :*: e2', TDouble)
            (TInt, TDouble) -> pure (e1' :*: e2', TDouble)
            (TDouble, TDouble) -> pure (e1' :*: e2', TDouble)
            (TString, _)       -> erro "Tipo String não compativel com a operação.\n" (e1' :*: e2', TString)
            (_, TString)       -> erro "Tipo String não compativel com a operação.\n" (e1' :*: e2', TString)
            (_, TVoid)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :*: e2', TVoid)
            (TVoid, _)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :*: e2', TVoid)
verificaExpr tfuns tvars (e1:/:e2) =
    do  (e1', t1) <- verificaExpr tfuns tvars e1
        (e2', t2) <- verificaExpr tfuns tvars e2

        case (t1, t2) of
            (TInt, TInt) -> pure (e1' :/: e2', TInt)
            (TDouble, TInt) -> pure (e1' :/: e2', TDouble)
            (TInt, TDouble) -> pure (e1' :/: e2', TDouble)
            (TDouble, TDouble) -> pure (e1' :/: e2', TDouble)
            (TString, _)       -> erro "Tipo String não compativel com a operação.\n" (e1' :/: e2', TString)
            (_, TString)       -> erro "Tipo String não compativel com a operação.\n" (e1' :/: e2', TString)
            (_, TVoid)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :/: e2', TVoid)
            (TVoid, _)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :/: e2', TVoid)

verificaExprR tfuns tvars (e1:==:e2) =
    do  (e1', t1) <- verificaExpr tfuns tvars e1
        (e2', t2) <- verificaExpr tfuns tvars e2

        case (t1, t2) of
                (TInt, TInt) -> pure (e1' :==: e2', TInt)
                (TDouble, TInt) -> pure (e1' :==: e2', TDouble)
                (TInt, TDouble) -> pure (e1' :==: e2', TDouble)
                (TDouble, TDouble) -> pure (e1' :==: e2', TDouble)
                (TString, _)       -> erro "Tipo String não compativel com a operação.\n" (e1' :==: e2', TString)
                (_, TString)       -> erro "Tipo String não compativel com a operação.\n" (e1' :==: e2', TString)
                (_, TVoid)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :==: e2', TVoid)
                (TVoid, _)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :==: e2', TVoid)
verificaExprR tfuns tvars (e1:/=:e2) =
    do  (e1', t1) <- verificaExpr tfuns tvars e1
        (e2', t2) <- verificaExpr tfuns tvars e2

        case (t1, t2) of
                (TInt, TInt) -> pure (e1' :/=: e2', TInt)
                (TDouble, TInt) -> pure (e1' :/=: e2', TDouble)
                (TInt, TDouble) -> pure (e1' :/=: e2', TDouble)
                (TDouble, TDouble) -> pure (e1' :/=: e2', TDouble)
                (TString, _)       -> erro "Tipo String não compativel com a operação.\n" (e1' :/=: e2', TString)
                (_, TString)       -> erro "Tipo String não compativel com a operação.\n" (e1' :/=: e2', TString)
                (_, TVoid)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :/=: e2', TVoid)
                (TVoid, _)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :/=: e2', TVoid)
verificaExprR tfuns tvars (e1:<:e2) =
    do  (e1', t1) <- verificaExpr tfuns tvars e1
        (e2', t2) <- verificaExpr tfuns tvars e2

        case (t1, t2) of
                (TInt, TInt) -> pure (e1' :<: e2', TInt)
                (TDouble, TInt) -> pure (e1' :<: e2', TDouble)
                (TInt, TDouble) -> pure (e1' :<: e2', TDouble)
                (TDouble, TDouble) -> pure (e1' :<: e2', TDouble)
                (TString, _)       -> erro "Tipo String não compativel com a operação.\n" (e1' :<: e2', TString)
                (_, TString)       -> erro "Tipo String não compativel com a operação.\n" (e1' :<: e2', TString)
                (_, TVoid)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :<: e2', TVoid)
                (TVoid, _)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :<: e2', TVoid)
verificaExprR tfuns tvars (e1:>:e2) =
    do  (e1', t1) <- verificaExpr tfuns tvars e1
        (e2', t2) <- verificaExpr tfuns tvars e2

        case (t1, t2) of
                (TInt, TInt) -> pure (e1' :>: e2', TInt)
                (TDouble, TInt) -> pure (e1' :>: e2', TDouble)
                (TInt, TDouble) -> pure (e1' :>: e2', TDouble)
                (TDouble, TDouble) -> pure (e1' :>: e2', TDouble)
                (TString, _)       -> erro "Tipo String não compativel com a operação.\n" (e1' :>: e2', TString)
                (_, TString)       -> erro "Tipo String não compativel com a operação.\n" (e1' :>: e2', TString)
                (_, TVoid)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :>: e2', TVoid)
                (TVoid, _)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :>: e2', TVoid)
verificaExprR tfuns tvars (e1:<=:e2) =
    do  (e1', t1) <- verificaExpr tfuns tvars e1
        (e2', t2) <- verificaExpr tfuns tvars e2

        case (t1, t2) of
                (TInt, TInt) -> pure (e1' :<=: e2', TInt)
                (TDouble, TInt) -> pure (e1' :<=: e2', TDouble)
                (TInt, TDouble) -> pure (e1' :<=: e2', TDouble)
                (TDouble, TDouble) -> pure (e1' :<=: e2', TDouble)
                (TString, _)       -> erro "Tipo String não compativel com a operação.\n" (e1' :<=: e2', TString)
                (_, TString)       -> erro "Tipo String não compativel com a operação.\n" (e1' :<=: e2', TString)
                (_, TVoid)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :<=: e2', TVoid)
                (TVoid, _)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :<=: e2', TVoid)
verificaExprR tfuns tvars (e1:>=:e2) =
    do  (e1', t1) <- verificaExpr tfuns tvars e1
        (e2', t2) <- verificaExpr tfuns tvars e2

        case (t1, t2) of
                (TInt, TInt) -> pure (e1' :>=: e2', TInt)
                (TDouble, TInt) -> pure (e1' :>=: e2', TDouble)
                (TInt, TDouble) -> pure (e1' :>=: e2', TDouble)
                (TDouble, TDouble) -> pure (e1' :>=: e2', TDouble)
                (TString, _)       -> erro "Tipo String não compativel com a operação.\n" (e1' :>=: e2', TString)
                (_, TString)       -> erro "Tipo String não compativel com a operação.\n" (e1' :>=: e2', TString)
                (_, TVoid)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :>=: e2', TVoid)
                (TVoid, _)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :>=: e2', TVoid)

verificaExprL tfuns tvars (el1:&:el2) =
    do  el1' <- verificaExprL tfuns tvars el1
        el2' <- verificaExprL tfuns tvars el2
        pure (el1' :&: el2')
verificaExprL tfuns tvars (el1:|:el2) =
    do  el1' <- verificaExprL tfuns tvars el1
        el2' <- verificaExprL tfuns tvars el2
        pure (el1' :|: el2')
verificaExprL tfuns tvars (Not el) =
    do  el' <- verificaExprL tfuns tvars el
        pure (Not el')
verificaExprL tfuns tvars (Rel er) =
    do  (er', t) <- verificaExprR tfuns tvars er
        pure (Rel er')

verificaComandos tfuns tvars f (If el bl1 bl2) =
    do  el' <- verificaExprL tfuns tvars el
        bl1' <- mapM (verificaComandos tfuns tvars f) bl1
        bl2' <- mapM (verificaComandos tfuns tvars f) bl2
        pure (If el' bl1' bl2')
verificaComandos tfuns tvars f (While el bl) =
    do  el' <- verificaExprL tfuns tvars el
        bl' <- mapM (verificaComandos tfuns tvars f) bl
        pure (While el' bl')
verificaComandos tfuns tvars f (DoWhile el bl) =
    do  el' <- verificaExprL tfuns tvars el
        bl' <- mapM (verificaComandos tfuns tvars f) bl
        pure (DoWhile el' bl')
verificaComandos tfuns tvars f (Atrib id e) =
    do  (e1, t1) <- verificaExpr tfuns tvars (IdVar id)
        (e2, t2) <- verificaExpr tfuns tvars e

        case (t1, t2) of
            (TInt,TInt) -> pure (Atrib id e2)
            (TDouble, TDouble) -> pure (Atrib id e2)
            (TString,TString) -> pure (Atrib id e2)
            (TInt, TDouble) -> warning("Atribuicao de Double para Int na variável "++id++".\n") (Atrib id (DoubleInt e2))
            (TDouble, TInt) -> warning("Atribuicao de Int para Double na variável "++id++".\n") (Atrib id (IntDouble e2))
            (TString, _) -> erro("Atribuicao invalida na variável "++id++".\n") (Atrib id e2)
            (_,TString) -> erro("Atribuicao invalida na variável "++id++".\n") (Atrib id e2)
            (TVoid,_ ) -> erro("Atribuicao invalida na variável "++id++".\n") (Atrib id e2)
            (_,TVoid) -> erro("Atribuicao invalida na variável "++id++".\n") (Atrib id e2)

verificaComandos tfuns tvars f (Ret e) =
    case e of
        Nothing   -> do
            (i, argumentos_esperados, t1) <- recuperaFuncao tfuns f
            if t1 == TVoid then pure (Ret Nothing)
            else erro("Tipo de retorno incompativel na função "++f++".\n") (Ret Nothing)

        Just expr -> do
            (i, argumentos_esperados, t1) <- recuperaFuncao tfuns f
            (e2, t2) <- verificaExpr tfuns tvars expr

            case (t1, t2) of
                (TInt,TInt) -> pure (Ret (Just expr))
                (TDouble, TDouble) -> pure (Ret (Just expr))
                (TString,TString) -> pure (Ret (Just expr))
                (TInt, TDouble) -> warning("Tipo Double para Tnt no retorno da função "++f++".\n") (Ret (Just (DoubleInt expr)))
                (TDouble, TInt) -> warning("Tipo Int para Double no retorno da função "++f++".\n") (Ret (Just (IntDouble expr)))
                (TString, _) -> erro("Tipo de retorno incompativel na função "++f++".\n") (Ret (Just expr))
                (_,TString) -> erro("Tipo de retorno incompativel na função "++f++".\n") (Ret (Just expr))
                (TVoid,_ ) -> erro("Tipo de retorno incompativel na função "++f++".\n") (Ret (Just expr))
                (_,TVoid) -> erro("Tipo de retorno incompativel na função "++f++".\n") (Ret (Just expr))

verificaComandos tfuns tvars f (Leitura id) =
    do  (e, t) <- verificaExpr tfuns tvars (IdVar id)
        case e of
            IdVar w -> pure (Leitura w)
verificaComandos tfuns tvars f (Imp e) =
    do  (e', t) <- verificaExpr tfuns tvars e
        pure (Imp e')
verificaComandos tfuns tvars f (Proc id exprs) =
    do  exprs' <- mapM (verificaExpr tfuns tvars) exprs
        (i, argumentos_esperados, t) <- recuperaFuncao tfuns id
        argumentos_verificados <- verificaArgumentos f argumentos_esperados exprs'
        pure (Proc id argumentos_verificados)

recuperaFuncao [] "main" = pure ("main",[],TVoid)
recuperaFuncao [] id = erro("Função "++id++" inexistente\n") (id,[],TVoid)
recuperaFuncao ((i:->:(args, t)):tfuns) id =
    if id == i then pure (i, args, t)
    else recuperaFuncao tfuns id

verificaArgumentos fid [] [] = pure []
verificaArgumentos fid [] _  = erro("Quantidade inválida de argumentos na função "++fid++".\n") []
verificaArgumentos fid _ []  = erro("Quantidade inválida de argumentos na função "++fid++".\n") []
verificaArgumentos fid ((i:#:t1):tvars) ((e,t2):exprs) =
    case (t1, t2) of
        (TInt,TInt) -> (:) e <$> verificaArgumentos fid tvars exprs
        (TDouble, TDouble) -> (:) e <$> verificaArgumentos fid tvars exprs
        (TString,TString) -> (:) e <$> verificaArgumentos fid tvars exprs
        (TInt, TDouble) -> (:) <$> warning("Atribuicao de Double para Int na chamada da funcao "++fid++".\n") (DoubleInt e) <*> verificaArgumentos fid tvars exprs
        (TDouble, TInt) -> (:) <$> warning("Atribuicao de Int para Double na chamada da funcao "++fid++".\n") (IntDouble e) <*> verificaArgumentos fid tvars exprs
        (TString, _) -> (:) <$> erro("Atribuicao invalida na chamada da funcao "++fid++".\n") e <*> verificaArgumentos fid tvars exprs
        (_,TString) -> (:) <$> erro("Atribuicao invalida na chamada da funcao "++fid++".\n") e <*> verificaArgumentos fid tvars exprs
        (TVoid,_ ) -> (:) <$> erro("Atribuicao invalida na chamada da funcao "++fid++".\n") e <*> verificaArgumentos fid tvars exprs
        (_,TVoid) -> (:) <$> erro("Atribuicao invalida na chamada da funcao "++fid++".\n") e <*> verificaArgumentos fid tvars exprs

verificaBloco tfuns (fid, tvars, comandos) =
    do
        (i, args, t) <- recuperaFuncao tfuns fid
        do  verificaVariaveisDuplicadas (tvars++args)
            cmds <- mapM (verificaComandos tfuns (tvars++args) fid) comandos
            pure (fid, tvars, cmds)

existeVar a [] = False
existeVar a ((id :#: t):tvars) = a == id || existeVar a tvars

verificaVariaveisDuplicadas [] = pure ()
verificaVariaveisDuplicadas ((id :#: t):tvars) = if existeVar id tvars then erro ("Variável "++id++" duplicadas.\n") () else verificaVariaveisDuplicadas tvars

existeFuncao a [] = False
existeFuncao a ((id :->: (var:vs,t)):fs) = a == id || existeFuncao a fs

verificaFuncoesDuplicadas [] = pure ()
verificaFuncoesDuplicadas ((id :->: (var:vs,t)):fs) = if existeFuncao id fs then erro ("Função "++id++" duplicada.\n") () else verificaFuncoesDuplicadas fs


verificaPrograma (Prog tfuns funcoes var_principal bloco_principal) =
    do
        verificaFuncoesDuplicadas tfuns
        fs <- mapM (verificaBloco tfuns) funcoes
        (fid, tvars, bloco) <- verificaBloco tfuns ("main", var_principal, bloco_principal)
        pure (Prog tfuns fs var_principal bloco)
