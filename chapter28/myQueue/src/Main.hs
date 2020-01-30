module Main where

import Criterion.Main
import Data.Sequence hiding (reverse, empty)

class Que f where
  empty :: f a

  -- adds an item
  push :: a -> f a -> f a
  pop :: f a -> Maybe (a, f a)


-- From Okasaki's Purely Functional Data Structures
data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show)

instance Que Queue where
  empty = Queue [] []

  push a (Queue enq deq) = Queue (a : enq) deq

  pop (Queue [] []) = Nothing
  pop (Queue enq (x:xs)) = Just (x, Queue enq xs)
  pop (Queue xs _) = pop (Queue [] (reverse xs))

-- ListQueue
newtype ListQueue a = ListQueue [a] deriving (Eq, Show)

instance Que ListQueue where
  empty = ListQueue []

  push x (ListQueue xs) = ListQueue (xs ++ [x])

  pop (ListQueue []) = Nothing
  pop (ListQueue (x:xs)) = Just (x, ListQueue xs)

-- Sequence
instance Que Seq where
  empty = fromList []
  push x seq = seq |> x
  pop seq =
    case viewl seq of
      EmptyL -> Nothing
      (x :< xs) -> Just (x, xs)

-- benchmark
constructQueue :: Que q => Int -> q Int
constructQueue n = go n empty
  where go 0 q = q
        go n q = go (n - 1) (push n q)

headQueue :: Que q => q Int -> Int
headQueue q = go q 0
  where go q defVal = case pop q of
          Nothing -> defVal
          Just (x, q') -> go q' x

main :: IO ()
main = do
  putStrLn . show $ pop (push 4 (push 3 (empty :: Queue Int)))
  putStrLn . show $ pop (push 4 (push 3 (empty :: Seq Int)))
  putStrLn . show $ pop (push 4 (push 3 (empty :: ListQueue Int)))

  defaultMain [
      bench "queue" $ whnf ((headQueue :: Queue Int -> Int) . constructQueue) 12345
    , bench "sequence" $ whnf ((headQueue :: Seq Int -> Int) . constructQueue) 12345
    , bench "list" $ whnf ((headQueue :: ListQueue Int -> Int) . constructQueue) 12345
    ]
