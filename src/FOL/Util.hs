module FOL.Util where

import qualified Data.Set as S

import           FOL.Base

free :: FOL -> S.Set String
free = \case
  T -> S.empty
  F -> S.empty
  Not fol -> free fol
  lhs :& rhs -> free lhs `S.union` free rhs
  lhs :| rhs -> free lhs `S.union` free rhs
  lhs :> rhs -> free lhs `S.union` free rhs
  Forall s fol -> free fol S.\\ S.singleton s
  Exists s fol -> free fol S.\\ S.singleton s
  Predicate s tes -> S.unions $ free' <$> tes
  where
    free' :: Term -> S.Set String
    free' = \case
      Constant s -> S.empty
      Var s -> S.singleton s
      Function s tes -> S.unions $ free' <$> tes

renameFree :: String -> String -> FOL -> FOL
renameFree old new = \case
  T -> T
  F -> F
  Not fol -> Not $ renameFree old new fol
  lhs :& rhs -> renameFree old new lhs :& renameFree old new rhs
  lhs :| rhs -> renameFree old new lhs :| renameFree old new rhs
  lhs :> rhs -> renameFree old new lhs :> renameFree old new rhs
  Forall s fol -> if s == old
                  then Forall s fol
                  else Forall s $ renameFree old new fol
  Exists s fol -> if s == old
                  then Exists s fol
                  else Forall s $ renameFree old new fol
  Predicate s tes -> Predicate s $ renameFree' old new <$> tes
  where
    renameFree' :: String -> String -> Term -> Term
    renameFree' old new = \case
      Constant s -> Constant s
      Var s -> Var $ if s == old then new else s
      Function s tes -> Function s $ renameFree' old new <$> tes

bound :: FOL -> S.Set String
bound = \case
  T -> S.empty
  F -> S.empty
  Not fol -> bound fol
  lhs :& rhs -> bound lhs `S.union` bound rhs
  lhs :| rhs -> bound lhs `S.union` bound rhs
  lhs :> rhs -> bound lhs `S.union` bound rhs
  Forall s fol -> S.singleton s `S.union` bound fol
  Exists s fol -> S.singleton s `S.union` bound fol
  Predicate s tes -> S.empty

renameBound :: String -> String -> FOL -> FOL
renameBound old new = \case
  T -> T
  F -> F
  Not fol -> Not $ renameBound old new fol
  lhs :& rhs -> renameBound old new lhs :& renameBound old new rhs
  lhs :| rhs -> renameBound old new lhs :| renameBound old new rhs
  lhs :> rhs -> renameBound old new lhs :> renameBound old new rhs
  Forall s fol -> if s == old
                  then Forall new $ go old new fol
                  else Forall s $ renameBound old new fol
  Exists s fol -> if s == old
                  then Exists new $ go old new fol
                  else Forall s $ renameBound old new fol
  Predicate s tes -> Predicate s tes
  where
    go :: String -> String -> FOL -> FOL
    go old new = \case
      T -> T
      F -> F
      Not fol -> Not $ go old new fol
      lhs :& rhs -> go old new lhs :& go old new rhs
      lhs :| rhs -> go old new lhs :| go old new rhs
      lhs :> rhs -> go old new lhs :> go old new rhs
      Forall s fol -> if s == old then Forall s fol else Forall s $ go old new fol
      Exists s fol -> if s == old then Exists s fol else Forall s $ go old new fol
      Predicate s tes -> Predicate s $ go' old new <$> tes

    go' :: String -> String -> Term -> Term
    go' old new = \case
      Constant s -> Constant s
      Var s -> Var $ if s == old then new else s
      Function s tes -> Function s $ go' old new <$> tes

constants :: FOL -> S.Set String
constants = \case
  T -> S.empty
  F -> S.empty
  Not fol -> constants fol
  lhs :& rhs -> constants lhs `S.union` constants rhs
  lhs :| rhs -> constants lhs `S.union` constants rhs
  lhs :> rhs -> constants lhs `S.union` constants rhs
  Forall s fol -> constants fol
  Exists s fol -> constants fol
  Predicate s tes -> S.unions $ constants' <$> tes
  where
    constants' :: Term -> S.Set String
    constants' = \case
      Constant s -> S.singleton s
      Var s -> S.empty
      Function s tes -> S.unions $ constants' <$> tes

variables :: FOL -> S.Set String
variables = \case
  T -> S.empty
  F -> S.empty
  Not fol -> variables fol
  lhs :& rhs -> variables lhs `S.union` variables rhs
  lhs :| rhs -> variables lhs `S.union` variables rhs
  lhs :> rhs -> variables lhs `S.union` variables rhs
  Forall s fol -> variables fol `S.union` S.singleton s
  Exists s fol -> variables fol `S.union` S.singleton s
  Predicate s tes -> S.unions $ variables' <$> tes
  where
    variables' :: Term -> S.Set String
    variables' = \case
      Constant s -> S.empty
      Var s -> S.singleton s
      Function s tes -> S.unions $ variables' <$> tes
