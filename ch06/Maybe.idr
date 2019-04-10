maybeAdd : Maybe Int -> Maybe Int -> Maybe Int
maybeAdd x y = case x of
                    Nothing => Nothing
                    (Just x_val) => case y of
                                         Nothing => Nothing
                                         (Just y_val) => Just (x_val + y_val)

maybeAdd2 : Maybe Int -> Maybe Int -> Maybe Int
maybeAdd2 x y = x >>= \x_val =>
                y >>= \y_val =>
                Just (x_val + y_val)

maybeAdd3 : Maybe Int -> Maybe Int -> Maybe Int
maybeAdd3 x y = do
  x_val <- x
  y_val <- y
  Just (x_val + y_val)
