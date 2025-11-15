allApp :: Monad m => m (a -> b) -> m a -> m b
allApp monadicFunc monadicValue = do
  func <- monadicFunc
  val <- monadicValue
  return (func val)
