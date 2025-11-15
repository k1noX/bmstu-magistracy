allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap func applicativeValue = pure func <*> applicativeValue