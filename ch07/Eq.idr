occurences : Eq ty => (item : ty) -> (values : List ty) -> Nat
occurences item [] = 0
occurences item (val :: vals) =
  case item == val of
       False => occurences item vals
       True => 1 + (occurences item vals)

data Matter = Solid | Liquid | Gas

Eq Matter where
  (==) Solid Solid = True
  (==) Liquid Liquid = True
  (==) Gas Gas = True
  (==) _ _ = False

  (/=) x y = not (x == y)
