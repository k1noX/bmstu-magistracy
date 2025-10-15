import Data.Bits (xor)
import Data.Char (ord, chr)

-- Cipher - класс шифра
class Cipher c where
  encode :: c -> String -> String
  decode :: c -> String -> String

-- prng генерирует значение
prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber

-- prngStream генерирует бесконечный список значений
prngStream :: Int -> Int -> Int -> Int -> [Int]
prngStream a b maxNumber seed = next : prngStream a b maxNumber next
  where
    next = prng a b maxNumber seed

-- StreamCipher - потоковый шифр
data StreamCipher = StreamCipher
  { scA :: Int
  , scB :: Int
  , scMax :: Int
  , scSeed :: Int
  }

cipherWithStream :: [Int] -> String -> String
cipherWithStream keyStream text = zipWith go text keyStream
  where
    go c k = chr (ord c `xor` (k `mod` 256))

instance Cipher StreamCipher where
  decode = encode
  encode cipher = cipherWithStream (prngStream (scA cipher) (scB cipher) (scMax cipher) (scSeed cipher))

myCipher :: StreamCipher
myCipher = StreamCipher
  { scA = 1337
  , scB = 7
  , scMax = 1000000
  , scSeed = 12345
  }

-- GHCi:
-- > let secret = encode myCipher "Hello, Jean-Paul!"
-- > decode myCipher secret
-- "Hello, Jean-Paul!"
