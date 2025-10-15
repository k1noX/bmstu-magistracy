import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

data TS a = TS [Int] [Maybe a]

createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
  where completeTimes = [minimum times .. maximum times]
        timeValueMap = Map.fromList (zip times values)
        extendedValues = map (`Map.lookup` timeValueMap) completeTimes

fileToTS :: [(Int, a)] -> TS a
fileToTS tvPairs = createTS times values
  where (times, values) = unzip tvPairs

showTVPair :: Show a => Int -> Maybe a -> String
showTVPair time (Just value) = mconcat [show time, "|", show value, "\n"]
showTVPair time Nothing = mconcat [show time, "|NA\n"]

instance Show a => Show (TS a) where
  show (TS times values) = mconcat rows
    where rows = zipWith showTVPair times values

insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing) = myMap
insertMaybePair myMap (key, Just value) = Map.insert key value myMap

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
  where bothTimes = mconcat [t1, t2]
        completeTimes = [minimum bothTimes .. maximum bothTimes]
        tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
        updatedMap = foldl insertMaybePair tvMap (zip t2 v2)
        combinedValues = map (`Map.lookup` updatedMap) completeTimes

instance Semigroup (TS a) where
  (<>) = combineTS

instance Monoid (TS a) where
  mempty = TS [] []
  mappend = (<>)

mean :: (Real a) => [a] -> Double
mean xs = total/count
  where total = (realToFrac . sum) xs
        count = (realToFrac . length) xs

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS times values) = if all (== Nothing) values
                           then Nothing
                           else Just avg
  where justVals = filter isJust values
        cleanVals = map fromJust justVals
        avg = mean cleanVals

type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare func = newFunc
  where newFunc (i1, Nothing) (i2, Nothing) = (i1, Nothing)
        newFunc (_, Nothing) (i, val) = (i, val)
        newFunc (i, val) (_, Nothing) = (i, val)
        newFunc (i1, Just val1) (i2, Just val2) = if func val1 val2 == val1
                                                  then (i1, Just val1)
                                                  else (i2, Just val2)

compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS func (TS [] []) = Nothing
compareTS func (TS times values) = if all (== Nothing) values
                                      then Nothing
                                      else Just best
  where pairs = zip times values
        best = foldl (makeTSCompare func) (0, Nothing) pairs

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max


diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing _ = Nothing
diffPair _ Nothing = Nothing
diffPair (Just x) (Just y) = Just (x - y)

diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS times values) = TS times (Nothing : diffValues)
  where shiftValues = tail values
        diffValues = zipWith diffPair shiftValues values

meanMaybe :: (Real a) => [Maybe a] -> Maybe Double
meanMaybe vals = if Nothing `elem` vals
                 then Nothing
                 else Just avg
  where avg = mean (map fromJust vals)

movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAvg [] n = []
movingAvg vals n = if length nextVals == n
                   then meanMaybe nextVals : movingAvg restVals n
                   else []
  where nextVals = take n vals
        restVals = tail vals

movingAverageTS :: (Real a) => TS a -> Int -> TS Double
movingAverageTS (TS [] []) n = TS [] []
movingAverageTS (TS times values) n = TS times smoothedValues
  where ma = movingAvg values n
        nothings = replicate (n `div` 2) Nothing
        smoothedValues = mconcat [nothings, ma, nothings]

-- median находит медиану в TS
median :: (Real a, Ord a) => [a] -> Double
median xs
  | even len  = (realToFrac a + realToFrac b) / 2
  | otherwise = realToFrac (sorted !! mid)
  where
    xs' = map realToFrac xs
    sorted = sort xs'
    len = length xs
    mid = len `div` 2
    a = sorted !! (mid - 1)
    b = sorted !! mid

medianMaybe :: (Real a, Ord a) => [Maybe a] -> Maybe Double
medianMaybe vals =
  if Nothing `elem` vals
  then Nothing
  else Just (median (map fromJust vals))

movingMedian :: (Real a, Ord a) => [Maybe a] -> Int -> [Maybe Double]
movingMedian [] _ = []
movingMedian vals n =
  if length nextVals == n
  then medianMaybe nextVals : movingMedian restVals n
  else []
  where
    nextVals = take n vals
    restVals = tail vals

movingMedianTS :: (Real a, Ord a) => TS a -> Int -> TS Double
movingMedianTS (TS [] []) _ = TS [] []
movingMedianTS (TS times values) n = TS times smoothedValues
  where
    mm = movingMedian values n
    pad = replicate (n `div` 2) Nothing
    smoothedValues = mconcat [pad, mm, pad]

-- ratioPair считает отношение между парой в TS
ratioPair :: (Fractional a, Eq a) => Maybe a -> Maybe a -> Maybe a
ratioPair Nothing _ = Nothing
ratioPair _ Nothing = Nothing
ratioPair (Just x) (Just y) =
  if y == 0 then Nothing else Just (x / y)

-- ratioTS аналог diffTS, но вычисляется как отношение, а не разность
ratioTS :: (Fractional a, Eq a) => TS a -> TS a
ratioTS (TS [] []) = TS [] []
ratioTS (TS times values) = TS times (Nothing : ratioValues)
  where
    shiftValues = tail values
    ratioValues = zipWith ratioPair shiftValues values

-- stdDev вычисляет стандартное отклонение
stdDev :: (Real a) => [a] -> Double
stdDev xs = sqrt (mean squaredDeviations)
  where
    avg = mean xs
    deviations = map (\x -> realToFrac x - avg) xs
    squaredDeviations = map (**2) deviations

-- stdDevTS вычисляет стандартное отклонение для TS
stdDevTS :: (Real a) => TS a -> Maybe Double
stdDevTS (TS _ []) = Nothing
stdDevTS (TS _ values) =
  if all (== Nothing) values
  then Nothing
  else Just (stdDev cleanVals)
  where
    cleanVals = catMaybes values

-- timeRange получает полную временную линию, включающую две последовательности
timeRange :: TS a -> TS a -> [Int]
timeRange (TS t1 _) (TS t2 _) =
  if null t1 && null t2
    then []
    else [minT .. maxT]
  where
    minT = minimum (filter (const True) (t1 ++ t2)) 
    maxT = maximum (t1 ++ t2)

-- resample преобразует TS в заданную временную линию
resample :: [Int] -> TS a -> [Maybe a]
resample timeline (TS times values) = map lookupTime timeline
  where
    m = Map.fromList (zip times values) 
    lookupTime t = Map.findWithDefault Nothing t m

-- zipTSWith сжимает временной ряд, используя заданную функцию
zipTSWith :: (a -> a -> a) -> TS a -> TS a -> TS a
zipTSWith f ts1 ts2 = TS fullTimes resultVals
  where
    fullTimes = timeRange ts1 ts2
    vals1 = resample fullTimes ts1
    vals2 = resample fullTimes ts2
    resultVals = zipWith combine vals1 vals2
    combine (Just x) (Just y) = Just (f x y)
    combine _ _ = Nothing

addTS = zipTSWith (+)
subTS = zipTSWith (-)