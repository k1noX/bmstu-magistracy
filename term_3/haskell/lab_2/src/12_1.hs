{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
data RhType = Pos | Neg
  deriving (Show, Eq)

data ABOType = A | B | AB | O
  deriving (Show, Eq)

-- Полная группа крови
data BloodType = BloodType ABOType RhType
  deriving (Show, Eq)

-- Пол пациента
data Sex = Male | Female
  deriving (Show, Eq)

-- Пациент
data Patient = Patient
  { name :: String
  , sex  :: Sex
  , age  :: Int
  , height :: Int
  , weight :: Int
  , bloodType :: BloodType
  }


canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo (BloodType AB _) (BloodType AB _) = True
canDonateTo _ _ = False

-- canPatientDonateTo проверяет может ли пациент быть донором крови для другого пациента
canPatientDonateTo :: Patient -> Patient -> Bool
canPatientDonateTo donor recipient = 
  canDonateTo (bloodType donor) (bloodType recipient)
