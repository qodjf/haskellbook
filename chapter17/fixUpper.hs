-- Q: const <$> Just "Hello" <*> "World"
x = const <$> Just "Hello" <*> pure "World"

-- Q: (,,,) Just 90 <*> Just 10 Just "Tierness" [1, 2, 3]
y = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
