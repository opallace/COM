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

verExpr tfuns [] (IdVar id) = erro("Variavel "++id++" não encontrada.\n") (IdVar id, TVoid)
verExpr tfuns ((i:#:(t, _)):tvars) (IdVar id) =
    if i == id then pure (IdVar id, t)
    else verExpr tfuns tvars (IdVar id)
verExpr tfuns tvars (Const (CInt x))    = pure (Const (CInt x), TInt)
verExpr tfuns tvars (Const (CDouble x)) = pure (Const (CDouble x), TDouble)
verExpr tfuns tvars (Lit s)    = pure (Lit s , TString)
verExpr tfuns tvars (IntDouble x)       = pure (IntDouble x, TDouble)
verExpr tfuns tvars (DoubleInt x)       = pure (DoubleInt x, TDouble)
verExpr tfuns tvars (Neg e) =
    do  (e', t) <- verExpr tfuns tvars e
        pure (Neg e', t)
verExpr tfuns tvars (Chamada id exprs) =
    do  exprs' <- mapM (verExpr tfuns tvars) exprs
        (i, argumentos_esperados, t) <- recuperaFuncao tfuns id
        argumentos_verificados <- verArgs id argumentos_esperados exprs'
        pure (Chamada id argumentos_verificados, t)
verExpr tfuns tvars (e1:+:e2) =
    do  (e1', t1) <- verExpr tfuns tvars e1
        (e2', t2) <- verExpr tfuns tvars e2

        case (t1, t2) of
            (TInt, TInt)       -> pure (e1' :+: e2', TInt)
            (TDouble, TInt)    -> pure (e1' :+: e2', TDouble)
            (TInt, TDouble)    -> pure (e1' :+: e2', TDouble)
            (TDouble, TDouble) -> pure (e1' :+: e2', TDouble)
            (TString, _)       -> erro "Tipo String não compativel com a operação.\n" (e1' :+: e2', TString)
            (_, TString)       -> erro "Tipo String não compativel com a operação.\n" (e1' :+: e2', TString)
            (_, TVoid)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :+: e2', TVoid)
            (TVoid, _)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :+: e2', TVoid)
verExpr tfuns tvars (e1:-:e2) =
    do  (e1', t1) <- verExpr tfuns tvars e1
        (e2', t2) <- verExpr tfuns tvars e2

        case (t1, t2) of
            (TInt, TInt) -> pure (e1' :-: e2', TInt)
            (TDouble, TInt) -> pure (e1' :-: e2', TDouble)
            (TInt, TDouble) -> pure (e1' :-: e2', TDouble)
            (TDouble, TDouble) -> pure (e1' :-: e2', TDouble)
            (TString, _)       -> erro "Tipo String não compativel com a operação.\n" (e1' :-: e2', TString)
            (_, TString)       -> erro "Tipo String não compativel com a operação.\n" (e1' :-: e2', TString)
            (_, TVoid)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :-: e2', TVoid)
            (TVoid, _)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :-: e2', TVoid)
verExpr tfuns tvars (e1:*:e2) =
    do  (e1', t1) <- verExpr tfuns tvars e1
        (e2', t2) <- verExpr tfuns tvars e2

        case (t1, t2) of
            (TInt, TInt) -> pure (e1' :*: e2', TInt)
            (TDouble, TInt) -> pure (e1' :*: e2', TDouble)
            (TInt, TDouble) -> pure (e1' :*: e2', TDouble)
            (TDouble, TDouble) -> pure (e1' :*: e2', TDouble)
            (TString, _)       -> erro "Tipo String não compativel com a operação.\n" (e1' :*: e2', TString)
            (_, TString)       -> erro "Tipo String não compativel com a operação.\n" (e1' :*: e2', TString)
            (_, TVoid)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :*: e2', TVoid)
            (TVoid, _)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :*: e2', TVoid)
verExpr tfuns tvars (e1:/:e2) =
    do  (e1', t1) <- verExpr tfuns tvars e1
        (e2', t2) <- verExpr tfuns tvars e2

        case (t1, t2) of
            (TInt, TInt) -> pure (e1' :/: e2', TInt)
            (TDouble, TInt) -> pure (e1' :/: e2', TDouble)
            (TInt, TDouble) -> pure (e1' :/: e2', TDouble)
            (TDouble, TDouble) -> pure (e1' :/: e2', TDouble)
            (TString, _)       -> erro "Tipo String não compativel com a operação.\n" (e1' :/: e2', TString)
            (_, TString)       -> erro "Tipo String não compativel com a operação.\n" (e1' :/: e2', TString)
            (_, TVoid)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :/: e2', TVoid)
            (TVoid, _)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :/: e2', TVoid)

verExprR tfuns tvars (e1:==:e2) =
    do  (e1', t1) <- verExpr tfuns tvars e1
        (e2', t2) <- verExpr tfuns tvars e2

        case (t1, t2) of
                (TInt, TInt) -> pure (e1' :==: e2', TInt)
                (TDouble, TInt) -> pure (e1' :==: e2', TDouble)
                (TInt, TDouble) -> pure (e1' :==: e2', TDouble)
                (TDouble, TDouble) -> pure (e1' :==: e2', TDouble)
                (TString, _)       -> erro "Tipo String não compativel com a operação.\n" (e1' :==: e2', TString)
                (_, TString)       -> erro "Tipo String não compativel com a operação.\n" (e1' :==: e2', TString)
                (_, TVoid)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :==: e2', TVoid)
                (TVoid, _)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :==: e2', TVoid)
verExprR tfuns tvars (e1:/=:e2) =
    do  (e1', t1) <- verExpr tfuns tvars e1
        (e2', t2) <- verExpr tfuns tvars e2

        case (t1, t2) of
                (TInt, TInt) -> pure (e1' :/=: e2', TInt)
                (TDouble, TInt) -> pure (e1' :/=: e2', TDouble)
                (TInt, TDouble) -> pure (e1' :/=: e2', TDouble)
                (TDouble, TDouble) -> pure (e1' :/=: e2', TDouble)
                (TString, _)       -> erro "Tipo String não compativel com a operação.\n" (e1' :/=: e2', TString)
                (_, TString)       -> erro "Tipo String não compativel com a operação.\n" (e1' :/=: e2', TString)
                (_, TVoid)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :/=: e2', TVoid)
                (TVoid, _)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :/=: e2', TVoid)
verExprR tfuns tvars (e1:<:e2) =
    do  (e1', t1) <- verExpr tfuns tvars e1
        (e2', t2) <- verExpr tfuns tvars e2

        case (t1, t2) of
                (TInt, TInt) -> pure (e1' :<: e2', TInt)
                (TDouble, TInt) -> pure (e1' :<: e2', TDouble)
                (TInt, TDouble) -> pure (e1' :<: e2', TDouble)
                (TDouble, TDouble) -> pure (e1' :<: e2', TDouble)
                (TString, _)       -> erro "Tipo String não compativel com a operação.\n" (e1' :<: e2', TString)
                (_, TString)       -> erro "Tipo String não compativel com a operação.\n" (e1' :<: e2', TString)
                (_, TVoid)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :<: e2', TVoid)
                (TVoid, _)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :<: e2', TVoid)
verExprR tfuns tvars (e1:>:e2) =
    do  (e1', t1) <- verExpr tfuns tvars e1
        (e2', t2) <- verExpr tfuns tvars e2

        case (t1, t2) of
                (TInt, TInt) -> pure (e1' :>: e2', TInt)
                (TDouble, TInt) -> pure (e1' :>: e2', TDouble)
                (TInt, TDouble) -> pure (e1' :>: e2', TDouble)
                (TDouble, TDouble) -> pure (e1' :>: e2', TDouble)
                (TString, _)       -> erro "Tipo String não compativel com a operação.\n" (e1' :>: e2', TString)
                (_, TString)       -> erro "Tipo String não compativel com a operação.\n" (e1' :>: e2', TString)
                (_, TVoid)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :>: e2', TVoid)
                (TVoid, _)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :>: e2', TVoid)
verExprR tfuns tvars (e1:<=:e2) =
    do  (e1', t1) <- verExpr tfuns tvars e1
        (e2', t2) <- verExpr tfuns tvars e2

        case (t1, t2) of
                (TInt, TInt) -> pure (e1' :<=: e2', TInt)
                (TDouble, TInt) -> pure (e1' :<=: e2', TDouble)
                (TInt, TDouble) -> pure (e1' :<=: e2', TDouble)
                (TDouble, TDouble) -> pure (e1' :<=: e2', TDouble)
                (TString, _)       -> erro "Tipo String não compativel com a operação.\n" (e1' :<=: e2', TString)
                (_, TString)       -> erro "Tipo String não compativel com a operação.\n" (e1' :<=: e2', TString)
                (_, TVoid)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :<=: e2', TVoid)
                (TVoid, _)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :<=: e2', TVoid)
verExprR tfuns tvars (e1:>=:e2) =
    do  (e1', t1) <- verExpr tfuns tvars e1
        (e2', t2) <- verExpr tfuns tvars e2

        case (t1, t2) of
                (TInt, TInt) -> pure (e1' :>=: e2', TInt)
                (TDouble, TInt) -> pure (e1' :>=: e2', TDouble)
                (TInt, TDouble) -> pure (e1' :>=: e2', TDouble)
                (TDouble, TDouble) -> pure (e1' :>=: e2', TDouble)
                (TString, _)       -> erro "Tipo String não compativel com a operação.\n" (e1' :>=: e2', TString)
                (_, TString)       -> erro "Tipo String não compativel com a operação.\n" (e1' :>=: e2', TString)
                (_, TVoid)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :>=: e2', TVoid)
                (TVoid, _)         -> erro "Tipo Void não compativel com a operação.\n" (e1' :>=: e2', TVoid)

verExprL tfuns tvars (el1:&:el2) =
    do  el1' <- verExprL tfuns tvars el1
        el2' <- verExprL tfuns tvars el2
        pure (el1' :&: el2')
verExprL tfuns tvars (el1:|:el2) =
    do  el1' <- verExprL tfuns tvars el1
        el2' <- verExprL tfuns tvars el2
        pure (el1' :|: el2')
verExprL tfuns tvars (Not el) =
    do  el' <- verExprL tfuns tvars el
        pure (Not el')
verExprL tfuns tvars (Rel er) =
    do  (er', t) <- verExprR tfuns tvars er
        pure (Rel er')

verCmd tfuns tvars f (If el bl1 bl2) =
    do  el' <- verExprL tfuns tvars el
        bl1' <- mapM (verCmd tfuns tvars f) bl1
        bl2' <- mapM (verCmd tfuns tvars f) bl2
        pure (If el' bl1' bl2')
verCmd tfuns tvars f (While el bl) =
    do  el' <- verExprL tfuns tvars el
        bl' <- mapM (verCmd tfuns tvars f) bl
        pure (While el' bl')
verCmd tfuns tvars f (DoWhile el bl) =
    do  el' <- verExprL tfuns tvars el
        bl' <- mapM (verCmd tfuns tvars f) bl
        pure (DoWhile el' bl')
verCmd tfuns tvars f (Atrib id e) =
    do  (e1, t1) <- verExpr tfuns tvars (IdVar id)
        (e2, t2) <- verExpr tfuns tvars e

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

verCmd tfuns tvars f (Ret e) =
    case e of
        Nothing   -> do
            (i, argumentos_esperados, t1) <- recuperaFuncao tfuns f
            if t1 == TVoid then pure (Ret Nothing)
            else erro("Tipo de retorno incompativel na função "++f++".\n") (Ret Nothing)

        Just expr -> do
            (i, argumentos_esperados, t1) <- recuperaFuncao tfuns f
            (e2, t2) <- verExpr tfuns tvars expr

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

verCmd tfuns tvars f (Leitura id) =
    do  (e, t) <- verExpr tfuns tvars (IdVar id)
        case e of
            IdVar w -> pure (Leitura w)
verCmd tfuns tvars f (Imp e) =
    do  (e', t) <- verExpr tfuns tvars e
        pure (Imp e')
verCmd tfuns tvars f (Proc id exprs) =
    do  exprs' <- mapM (verExpr tfuns tvars) exprs
        (i, argumentos_esperados, t) <- recuperaFuncao tfuns id
        argumentos_verificados <- verArgs f argumentos_esperados exprs'
        pure (Proc id argumentos_verificados)

recuperaFuncao [] "main" = pure ("main",[],TVoid)
recuperaFuncao [] id = erro("Função "++id++" inexistente\n") (id,[],TVoid)
recuperaFuncao ((i:->:(args, t)):tfuns) id =
    if id == i then pure (i, args, t)
    else recuperaFuncao tfuns id

verArgs fid [] [] = pure []
verArgs fid [] _  = erro("Quantidade inválida de argumentos na função "++fid++".\n") []
verArgs fid _ []  = erro("Quantidade inválida de argumentos na função "++fid++".\n") []
verArgs fid ((i:#:(t1, _)):tvars) ((e,t2):exprs) =
    case (t1, t2) of
        (TInt,TInt) -> (:) e <$> verArgs fid tvars exprs
        (TDouble, TDouble) -> (:) e <$> verArgs fid tvars exprs
        (TString,TString) -> (:) e <$> verArgs fid tvars exprs
        (TInt, TDouble) -> (:) <$> warning("Atribuicao de Double para Int na chamada da funcao "++fid++".\n") (DoubleInt e) <*> verArgs fid tvars exprs
        (TDouble, TInt) -> (:) <$> warning("Atribuicao de Int para Double na chamada da funcao "++fid++".\n") (IntDouble e) <*> verArgs fid tvars exprs
        (TString, _) -> (:) <$> erro("Atribuicao invalida na chamada da funcao "++fid++".\n") e <*> verArgs fid tvars exprs
        (_,TString) -> (:) <$> erro("Atribuicao invalida na chamada da funcao "++fid++".\n") e <*> verArgs fid tvars exprs
        (TVoid,_ ) -> (:) <$> erro("Atribuicao invalida na chamada da funcao "++fid++".\n") e <*> verArgs fid tvars exprs
        (_,TVoid) -> (:) <$> erro("Atribuicao invalida na chamada da funcao "++fid++".\n") e <*> verArgs fid tvars exprs

verBloco tfuns (fid, tvars, comandos) =
    do
        (i, args, t) <- recuperaFuncao tfuns fid
        do  verVariaveisDuplicadas (tvars++args)
            cmds <- mapM (verCmd tfuns (tvars++args) fid) comandos
            pure (fid, tvars, cmds)

existeVar a [] = False
existeVar a ((id :#: (t, _)):tvars) = a == id || existeVar a tvars

verVariaveisDuplicadas [] = pure ()
verVariaveisDuplicadas ((id :#: (t, _)):tvars) = if existeVar id tvars then erro ("Variável "++id++" duplicadas.\n") () else verVariaveisDuplicadas tvars

existeFuncao a [] = False
existeFuncao a ((id :->: (var:vs,t)):fs) = a == id || existeFuncao a fs

verFuncoesDuplicadas [] = pure ()
verFuncoesDuplicadas ((id :->: (var:vs,t)):fs) = if existeFuncao id fs then erro ("Função "++id++" duplicada.\n") () else verFuncoesDuplicadas fs

definePos _ [] = []
definePos n (i:#:(t,_):vs) = (i:#:(t,n)):if t == TDouble then definePos (n+2) vs else definePos (n+1) vs 

verProg (Prog tfuns funcoes var_principal bloco_principal) =
    do
        verFuncoesDuplicadas tfuns
        fs <- mapM (verBloco tfuns) funcoes
        (fid, tvars, bloco) <- verBloco tfuns ("main", var_principal, bloco_principal)
        pure (Prog tfuns fs (definePos 1 var_principal) bloco)
