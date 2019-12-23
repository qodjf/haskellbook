data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = case f a of
  Nothing -> Leaf
  Just (l, b, r) -> Node (unfold f l) b (unfold f r)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where
    f x = if x == n then Nothing else Just (x + 1, x, x + 1)
