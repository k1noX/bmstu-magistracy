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

-- showSex выводит пол в виде строки
showSex :: Sex -> String
showSex Male   = "male"
showSex Female = "female"

-- showBloodType выводит тип крови в виде строки (например, "B+")
showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO ++ showRh
  where
    showABO = case abo of
      A  -> "A"
      B  -> "B"
      AB -> "AB"
      O  -> "O"
    showRh = case rh of
      Pos -> "+"
      Neg -> "-"

-- patientSummary выводит информацию о пациенте
patientSummary :: Patient -> String
patientSummary p = unlines
  [ "**************"
  , "Имя пациента: " ++ name p
  , "Пол: " ++ showSex (sex p)
  , "Возраст: " ++ show (age p)
  , "Рост: " ++ show (height p) ++ "см"
  , "Вес: " ++ show (weight p) ++ "кг"
  , "Тип крови: " ++ showBloodType (bloodType p)
  , "**************"
  ]
