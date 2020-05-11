- height vs size
- foldl for automata?

- # of equivalence classes of regexes at each size?

- DFAs?
- union, intersection, negation
- equivalence?
  Hopcroft and Karp bisimulation algorithm
- JIT compile w/LLVM

  given a regex, use LLVM (a compiler framework) to generate code at runtime
  the SUPER EFFICIENTLY recognizes that regular expression

- refactor type classes for regular things

  class Regular a where
    empty, epsilon :: a
    char :: Char -> a
    seq :: a -> a -> a
    alt :: a -> a -> a
    star :: a -> a
    
    matches :: a -> Text -> Bool

- refactor Automaton class

  class Automaton a s where
    startState :: a -> s
    step :: a -> s -> c -> s
    accepting :: a -> s -> Bool

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
