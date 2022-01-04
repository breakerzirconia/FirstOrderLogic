module FOL.Base where

infixl 3 :&
infixl 2 :|
infixr 1 :>

data FOL
  = T
  | F
  | Not FOL
  | FOL :& FOL
  | FOL :| FOL
  | FOL :> FOL
  | Forall String FOL
  | Exists String FOL
  | Predicate String [Term]
  deriving (Eq)

instance Show FOL where
  show = \case
    T -> "⊤"
    F -> "⊥"
    Not fol -> "(¬ " <> show fol <> ")"
    lhs :& rhs -> "(" <> show lhs <> " ∧ " <> show rhs <> ")"
    lhs :| rhs -> "(" <> show lhs <> " ∨ " <> show rhs <> ")"
    lhs :> rhs -> "(" <> show lhs <> " → " <> show rhs <> ")"
    Forall s fol -> "(∀ " <> s <> " . " <> show fol <> ")"
    Exists s fol -> "(∃ " <> s <> " . " <> show fol <> ")"
    Predicate s tes -> s <> "(" <> listArgs tes <> ")"

data Term
  = Constant String
  | Var String
  | Function String [Term]
  deriving (Eq)

instance Show Term where
  show = \case
    Constant c -> c
    Var v -> v
    Function s tes -> s <> "(" <> listArgs tes <> ")"

listArgs :: [Term] -> String
listArgs [] = ""
listArgs (t : ts) = show t <> go ts
  where
    go []       = ""
    go (t : ts) = ", " <> show t <> go ts
