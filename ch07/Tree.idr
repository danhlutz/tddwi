data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

Eq elem => Eq (Tree elem) where
  (==) Empty Empty = True
  (==) (Node left e right) (Node left' e' right') =
    left == left' && e == e' && right == right'
  (==) _ _ = False

Functor Tree where
  map func Empty = Empty
  map func (Node l x r) =
    Node (map func l) (func x) (map func r)

Foldable Tree where
  foldr func init Empty = init
  foldr func init (Node left e right) =
    let leftfold = foldr func init left
        rightfold = foldr func leftfold right in
        func e rightfold

testTree : Tree Integer
testTree = Node
             (Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty))
             4
             (Node (Node Empty 5 Empty) 6 (Node Empty 7 Empty))
