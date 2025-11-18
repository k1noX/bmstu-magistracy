sizeData :: [Double]
sizeData = [12.0, 14.0]

costData :: [Double]
costData = [10.0, 15.0]

comparePizzas :: (Double, Double) -> (Double, Double) -> (Double, Double)
comparePizzas p1 p2 = p1

describePizza :: (Double, Double) -> String
describePizza (size, cost) = "Pizza size: " ++ show size ++ ", cost: " ++ show cost

listMain :: [String]
listMain = do
  size1 <- sizeData
  cost1 <- costData
  size2 <- sizeData
  cost2 <- costData
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)
