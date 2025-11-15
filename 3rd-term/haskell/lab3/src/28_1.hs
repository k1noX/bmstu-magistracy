import Text.Printf (printf)

type Lat = Double
type Long = Double
type LatLong = (Lat, Long)

haversine :: LatLong -> LatLong -> Double
haversine (lat1, long1) (lat2, long2) =
  let lat1Rad = lat1 * pi / 180
      long1Rad = long1 * pi / 180
      lat2Rad = lat2 * pi / 180
      long2Rad = long2 * pi / 180
      dlat = lat2Rad - lat1Rad
      dlong = long2Rad - long1Rad
      a = sin (dlat/2)^2 + cos lat1Rad * cos lat2Rad * sin (dlong/2)^2
      c = 2 * atan2 (sqrt a) (sqrt (1 - a))
  in 6371 * c

-- Реализация haversineIO без использования <*>
haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO ioLatLong1 ioLatLong2 = do
  latLong1 <- ioLatLong1
  latLong2 <- ioLatLong2
  return (haversine latLong1 latLong2)
