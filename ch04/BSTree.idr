data BSTree : Type -> Type where
  Empty : Ord elem => BSTree elem
  Node  : Ord elem => (left : BSTree elem) -> (val :elem) ->
                      (right : BSTree elem) -> BSTree elem


insert : elem -> BSTree elem -> BSTree elem
insert x Empty = Node Empty x Empty
insert x o@(Node left val right)
  = case compare x val of
         LT => Node (insert x left) val right
         EQ => o
         GT => Node left val (insert x right)
