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
