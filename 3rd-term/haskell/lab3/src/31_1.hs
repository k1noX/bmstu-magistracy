type Pizza = (Double, Double)

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size / 2)^2

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2
                      then p1
                      else p2
  where costP1 = costPerInch p1
        costP2 = costPerInch p2

describePizza :: Pizza -> String
describePizza (size, cost) = "The " ++ show size ++ " pizza " ++
                             "is cheaper at " ++ show costSqInch ++
                             " per square inch"
  where costSqInch = costPerInch (size, cost)

main :: IO ()
main =
  putStrLn "Введите размер первой пиццы" >>
  getLine >>= \size1 ->
  putStrLn "Введите стоимость первой пиццы" >>
  getLine >>= \cost1 ->
  putStrLn "Введите размер второй пиццы" >>
  getLine >>= \size2 ->
  putStrLn "Введите стоимость второй пиццы" >>
  getLine >>= \cost2 ->
  let pizza1 = (read size1, read cost1)
      pizza2 = (read size2, read cost2)
      betterPizza = comparePizzas pizza1 pizza2
  in putStrLn (describePizza betterPizza)