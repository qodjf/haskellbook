import Control.Applicative (liftA2)

-- Constant
data Constant a b =
  Constant a

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a

-- Two
data Two a b =
  Two a b

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

-- Three
data Three a b c =
  Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

-- Three'
data Three' a b =
  Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance Foldable (Three' a) where
  foldMap f (Three' _ b1 b2) = f b1 <> f b2

instance Traversable (Three' a) where
  traverse f (Three' a b1 b2) = liftA2 (Three' a) (f b1) (f b2)

-- Four'
data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ b1 b2 b3) = f b1 <> f b2 <> f b3

filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF pred = foldMap (\a -> if pred a then pure a else mempty)
