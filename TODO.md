- DFAs?
- union, intersection, negation
- equivalence?
- refactor type classes for regular things

  class Regular a where
    empty, epsilon :: a
    char :: Char -> a
    seq :: a -> a -> a
    alt :: a -> a -> a
    star :: a -> a
    
    matches :: a -> Text -> Bool

- less interesting/possibly pointless abstraction

  class StateSet set state where
    empty  :: set
    member :: state -> set -> Bool
    insert :: state -> set -> set
    union  :: set -> set -> set
    difference :: set -> set -> set
    foldr  :: (state -> b -> b) -> b -> set -> b

  class StateSet set state => Automaton set state char trans where
    states :: set
    delta :: state -> char -> trans
    accepting :: set
    startState :: state
