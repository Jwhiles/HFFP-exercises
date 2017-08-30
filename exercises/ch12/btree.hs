data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x =
  case (f x) of
    (Just (p, b, l)) -> Node (unfold f p) b (unfold f l)
    _ -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild = unfold (\i -> if i == 0 
                            then Nothing
                            else Just((i-1), i, (i-1)))
