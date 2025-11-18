allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM func monadicValue = do
  x <- monadicValue
  return (func x)
