-- chapter 12 section 1 exercises, take 2
import Control.Monad.State

-- 1
update : (stateType -> stateType) -> State stateType ()
update f = do
  current <- get
  put (f current)

increase : Nat -> State Nat ()
increase k = update (+k)

-- 2
data Tree a = Empty
            | Node (Tree a) a (Tree a)

testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))

countEmpty : Tree a -> State Nat ()
countEmpty Empty = do
  current <- get
  put (S current)
countEmpty (Node left val right) = do
  _ <- countEmpty left
  left_count <- get
  countEmpty right

-- 3
countEmptyNode : Tree a -> State (Nat, Nat) ()
countEmptyNode Empty = do
  (empties, nodes) <- get
  put (S empties, nodes)
countEmptyNode (Node left val right) = do
  _ <- countEmptyNode left
  (empties, nodes) <- get
  put (empties, S nodes)
  countEmptyNode right
