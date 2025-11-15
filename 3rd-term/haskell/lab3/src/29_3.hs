-- Сколько бутылок было изначально
initialBeers :: [Int]
initialBeers = [6, 12]

-- Сколько уже выпито
alreadyDrunk :: Int
alreadyDrunk = 4  -- вы и сосед по 2 бутылки

-- Сколько друзей может прийти
numFriends :: [Int]
numFriends = [2, 3]

-- Сколько выпьет один человек за ночь
beersPerPerson :: [Int]
beersPerPerson = [3, 4]

-- Сколько всего выпьют за ночь

-- Общее потребление сегодня:
totalNeededTonight :: [Int]
totalNeededTonight = (*) <$> numFriends <*> beersPerPerson

-- Остаток от вчерашней покупки:
remainingBeers :: [Int]
remainingBeers = (\initial -> initial - alreadyDrunk) <$> initialBeers

-- Для каждой комбинации: сколько не хватает?
-- Если остаток >= потребности → 0
-- Иначе → потребность - остаток
deficit :: [Int]
deficit = 
  [ max 0 (needed - remaining)
  | remaining <- remainingBeers
  , needed <- totalNeededTonight
  ]

-- Минимальное количество пива, которое нужно докупить, чтобы хватило всегда:
-- Это максимальный дефицит среди всех сценариев.
beersToBuy :: Int
beersToBuy = maximum deficit