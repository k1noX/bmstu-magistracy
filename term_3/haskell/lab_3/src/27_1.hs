data Box a = Box a deriving Show

-- Реализация Functor для Box
instance Functor Box where
  fmap f (Box x) = Box (f x)

-- Функция, которая превращает Box a в Box [a], содержащий n копий значения
morePresents :: Int -> Box a -> Box [a]
morePresents n = fmap (replicate n)
