-- Circle - круг, который задаётся радиусом
newtype Circle = Circle Double
  deriving (Show)

-- Square - квадрат, который задаётся длиной стороны
newtype Square = Square Double
  deriving (Show)

-- Rectangle - прямоугольник, который задаётся шириной и высотой
data Rectangle = Rectangle Double Double
  deriving (Show)

data Shape =
  ShapeCircle Circle
  | ShapeSquare Square
  | ShapeRectangle Rectangle
  deriving (Show)

area :: Shape -> Double
area (ShapeCircle (Circle r))           = pi * r * r
area (ShapeSquare (Square s))           = s * s
area (ShapeRectangle (Rectangle w h))   = w * h

perimeter :: Shape -> Double
perimeter (ShapeCircle (Circle r))         = 2 * pi * r
perimeter (ShapeSquare (Square s))         = 4 * s
perimeter (ShapeRectangle (Rectangle w h)) = 2 * (w + h)
