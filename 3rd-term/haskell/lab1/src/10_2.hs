{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}

type Robot = forall a. ((String, Int, Int) -> a) -> a

-- | Конструктор робота: принимает тройку (имя, атака, здоровье)
-- и возвращает "робота" — функцию, которая принимает сообщение (функцию)
-- и применяет его к внутреннему состоянию.
robot :: (String, Int, Int) -> Robot
robot (name, attack, hp) = \message ->
  message (name, attack, hp)

name :: (String, a, b) -> String
name (n, _, _) = n

attack :: (a, Int, b) -> Int
attack (_, a, _) = a

hp :: (a, b, Int) -> Int
hp (_, _, hp) = hp

getName :: Robot -> String
getName aRobot = aRobot name

getAttack :: Robot -> Int
getAttack aRobot = aRobot attack

getHP :: Robot -> Int
getHP aRobot = aRobot hp

setName :: Robot -> String -> Robot
setName aRobot newName =
  aRobot
    ( \(n, a, h) ->
        robot (newName, a, h)
    )

setAttack :: Robot -> Int -> Robot
setAttack aRobot newAttack =
  aRobot
    ( \(n, a, h) ->
        robot (n, newAttack, h)
    )

setHP :: Robot -> Int -> Robot
setHP aRobot newHP =
  aRobot
    ( \(n, a, h) ->
        robot (n, a, newHP)
    )

printRobot :: Robot -> String
printRobot aRobot =
  aRobot
    ( \(n, a, h) ->
        n
          ++ " attack HP:"
          ++ show a
          ++ " HP:"
          ++ show h
    )

damage :: Robot -> Int -> Robot
damage aRobot attackDamage =
  aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

fight :: Robot -> Robot -> Robot
fight aRobot defender = damage defender attack
  where
    attack =
      if getHP aRobot > 10
        then getAttack aRobot
        else 0

-- Далее идут дополнения из пункта "Расширение проекта"

-- Задание 1

-- | Используя map на списке роботов, получите количество жизни каждого робота в списке
getHPs :: [Robot] -> [Int]
getHPs robots = map getHP robots

-- Задание 2

-- | getFightWinner - функция с аннотацией типа, чтобы было возможно реализовать перестановки аргументов
getFightWinner :: Robot -> Robot -> Robot
getFightWinner robotA robotB =
  if getHP robotA > getHP robotB
    then robotA
    else robotB

-- | Функция multiroundFight, принимающая на вход двух роботов, заставляющую их драться в течение N раундов и
-- возвращающую победителя
multiroundFight :: Robot -> Robot -> Int -> Robot
multiroundFight robotA robotB count =
  case count of
    1 ->
      getFightWinner
        (fight robotB robotA)
        (fight robotA robotB)
    r ->
      multiroundFight
        (fight robotB robotA)
        (fight robotA robotB)
        (count - 1)

{-
Сценарий битвы:
robotA = robot("RA", 30, 90)
robotB = robot("RB", 30, 100)
printRobot (multiroundFight robotA robotB 3)
-}

-- Задание 3

robot1 :: Robot
robot1 = robot ("R1", 10, 50)

robot2 :: Robot
robot2 = robot ("R2", 12, 45)

robot3 :: Robot
robot3 = robot ("R3", 8, 60)

robots :: [Robot]
robots = [robot1, robot2, robot3]

boss :: Robot
boss = robot ("Boss", 20, 100)

fightBoss :: Robot -> Robot
fightBoss = fight boss

damagedRobots :: [Robot]
damagedRobots = map fightBoss robots

remainingHPs :: [Int]
remainingHPs = map getHP damagedRobots
