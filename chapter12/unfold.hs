myIterate :: (a -> a) -> a -> [a]
myIterate f s = s : (myIterate f (f s))

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f s = case f s of
  Nothing -> []
  Just (a, b) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just (a, f a))
