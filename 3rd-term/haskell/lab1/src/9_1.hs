-- | myElem - переопределение функции elem.
myElem vs v = length(filter (== v) vs) > 0
