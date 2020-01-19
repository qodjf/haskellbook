easyintegrate :: Fractional a => (a -> a) -> a -> a -> a
easyintegrate f a b = (f a + f b)* (b - a) / 2

intergrate :: Fractional a => (a -> a) -> a -> a -> [a]
intergrate f a b =
  let mid = (a + b) / 2
  in easyintegrate f a b : (map (uncurry (+)) $ zip (intergrate f a mid) (intergrate f mid b))


integ f a b fa fb =
  (fa + fb) * (b - a) / 2 : (map (uncurry (+)) $ zip (integ f a mid fa fm) (integ f mid b fm fb))
  where
    mid = (a + b) / 2
    fm = f mid
integrate' :: Fractional a => (a -> a) -> a -> a -> [a]
integrate' f a b = integ f a b (f a) (f b)

elimerror :: Floating a => a -> [a] -> [a]
elimerror n (a:b:rest) = (b * (2 ** n) - a) / (2 ** n - 1) : (elimerror n (b:rest))

order (a:b:c:rest) = round . log $ (a - c) / (b - c) - 1

improve :: (Floating a, RealFrac a) => [a] -> [a]
improve s = elimerror (fromIntegral (order s)) s

super :: (Floating a, RealFrac a) => [a] -> [a]
super s = map (\s -> s !! 1) (iterate improve s)
