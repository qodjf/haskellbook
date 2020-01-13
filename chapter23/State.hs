{-# LANGUAGE InstanceSigs #-}
module State where

newtype State s a =
  State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) =
    State $ \s -> let (a, s1) = g s in (f a, s1)

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)

  (<*>) :: State s (a -> b)
        -> State s a
        -> State s b
  (State sf) <*> (State g) =
    State $ \s ->
      let (f, s1) = sf s
          (a, s2) = g s1
      in (f a, s2)

instance Monad (State s) where
  return = pure

  (>>=) :: State s a
        -> (a -> State s b)
        -> State s b
  (State f) >>= g =
    State $ \s ->
      let (a, s1) = f s
          (State h) = g a
      in h s1

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

exec :: State s a -> s -> s
exec (State sa) s = snd $ sa s

eval :: State s a -> s -> a
eval (State sa) s = fst $ sa s

modify :: (s -> s) -> State s ()
modify f =
  State $ \s -> ((), f s)
