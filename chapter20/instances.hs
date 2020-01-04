-- Constant
data Constant a b =
  Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

-- Two
data Two a b =
  Two a b

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

-- Three
data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

-- Three'
data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ b1 b2) = f b1 <> f b2

-- Four'
data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ b1 b2 b3) = f b1 <> f b2 <> f b3

filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF pred = foldMap (\a -> if pred a then pure a else mempty)
