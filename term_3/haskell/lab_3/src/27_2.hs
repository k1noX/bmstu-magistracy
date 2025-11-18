data Box a = Box a deriving Show

instance Functor Box where
  fmap f (Box x) = Box (f x)

unwrap :: Box a -> a
unwrap (Box x) = x

myBox :: Box Int
myBox = Box 1