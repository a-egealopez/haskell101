import Data.Char (isLetter, toUpper, ord, chr)
import Data.List (group, sort, nub, elemIndices)
import Control.Applicative (ZipList(..))

class Monoid g => Group g where
  invert :: g -> g

-------------------- Datos y Álgebras --------------------

data Mode = Encrypt | Decrypt

newtype Shift = Shift Int deriving (Eq, Show)

instance Semigroup Shift where
    (Shift a) <> (Shift b) = Shift ((a + b) `mod` 26)

instance Monoid Shift where
    mempty = Shift 0

instance Group Shift where
  invert (Shift n) = Shift (-n `mod` 26)

-------------------- Lógica de transformación --------------------

applyMode :: Mode -> Shift -> Shift
applyMode Encrypt = id
applyMode Decrypt = invert

normalize :: String -> String
normalize str = toUpper <$> filter isLetter str

shiftChar :: Char -> Shift -> Char
shiftChar c (Shift n) = chr (ord 'A' + (ord c - ord 'A' + n) `mod` 26)

-------------------- Algoritmo de Vigenère --------------------

vigenereTransform :: Mode -> String -> String -> String
vigenereTransform mode key str = getZipList $ 
    shiftChar <$> ZipList (normalize str) 
              <*> ZipList (cycle shifts)
  where
    shifts = map toShift $ normalize key
    toShift c = applyMode mode $ Shift (ord c - ord 'A')

encrypt :: String -> String -> String
encrypt = vigenereTransform Encrypt

decrypt :: String -> String -> String
decrypt = vigenereTransform Decrypt

-------------------- Método de Kasiski --------------------

factorize :: Int -> [Int]
factorize n = sort . concat $ [ [x, n `div` x] | x <- [1..limit], n `mod` x == 0 ]
  where 
    limit = floor . sqrt . fromIntegral $ n

possibleLengths :: String -> [Int]
possibleLengths str = nub . concatMap factorize $ dists
  where
    t = normalize str
    trigrams = zip3 t (drop 1 t) (drop 2 t)
    dists = do
        (x:_) <- filter ((>1) . length) (group $ sort trigrams)
        let indices = elemIndices x trigrams
        zipWith (-) (drop 1 indices) indices

{--
------------------- EXPLICACIÓN DE dists --------------------
ghci> let trigrams = ["BAN", "ANA", "NAN", "ANA", "NAB", "ABA", "BAN"]
ghci> sort trigrams
["ABA","ANA","ANA","BAN","BAN","NAB","NAN"]
ghci> group $ sort trigrams
[["ABA"],["ANA","ANA"],["BAN","BAN"],["NAB"],["NAN"]]
ghci> filter ((>1) . length) (group $ sort trigrams)
[["ANA","ANA"],["BAN","BAN"]]
ghci> let ((x:_):_) = filter ((>1) . length) (group $ sort trigrams) -- Emulando el uso de <-
ghci> print x
"ANA"
ghci> let indices = elemIndices x trigrams
ghci> print indices
[1,3]
ghci> print $ drop 1 indices
[3]
ghci> zipWith (-) (drop 1 indices) indices
[2]
---------------------------- end ----------------------------
--}