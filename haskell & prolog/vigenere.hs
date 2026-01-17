import Data.Char (isLetter, toUpper, ord, chr)
import Data.List (group, sort, nub, elemIndices, sortOn, maximumBy)
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
    shiftChar <$> ZipList (str) 
              <*> ZipList (cycle shifts)
  where
    shifts = map toShift $ key
    toShift c = applyMode mode $ Shift (ord c - ord 'A')

encrypt :: String -> String -> String
encrypt key str = vigenereTransform Encrypt cKey cStr
  where
    cKey = normalize key 
    cStr = normalize str

decrypt :: String -> String -> String
decrypt = vigenereTransform Decrypt

-------------------- Método de Kasiski --------------------

factorize :: Int -> [Int]
factorize n = sort . concat $ [ [x, n `div` x] | x <- [1..limit], n `mod` x == 0 ]
  where 
    limit = floor . sqrt . fromIntegral $ n

possibleLengths :: String -> [Int]
possibleLengths str = concatMap (take 1) . sortOn (negate . length) . group $ sortedFactors
  where
    trigrams = zip3 str (drop 1 str) (drop 2 str)
    dists = do
        (x:_) <- filter ((>1) . length) $ group $ sort trigrams
        let indices = elemIndices x trigrams
        zipWith (-) (drop 1 indices) indices
    sortedFactors = sort . filter (\k -> k > 1 && k <= 20) . concatMap factorize $ dists


-------------------- Índice de Coincidencia --------------------


buildSubstrings :: Int -> String -> [String]
buildSubstrings k msg = [ [ msg !! j | j <- [i, i+k .. length msg - 1] ] | i <- [0 .. k-1]]

frequencies :: String -> [Int]
frequencies s = map length. group. sort $ s

indexOfCoincidence :: String -> Double
indexOfCoincidence s
  | n <= 1    = 0
  | otherwise = fromIntegral acc / fromIntegral (n * (n - 1))
  where
    freqs = frequencies s
    acc   = sum [ f * (f - 1) | f <- freqs ]
    n     = sum freqs
    
indexOfCoincidenceTest :: Int -> String -> [Double]
indexOfCoincidenceTest k message = map indexOfCoincidence substrings
  where
    substrings = buildSubstrings k (message)

textoCifrado :: String
textoCifrado =
  "UECWKDVLOTTVACKTPVGEZQMDAMRNPDDUXLBUICAMRHOECBHSPQLVIWO\
  \FFEAILPNTESMLDRUURIFAEQTTPXADWIAWLACCRPBHSRZIVQWOFROGTT\
  \NNXEVIVIBPDTTGAHVIACLAYKGJIEQHGECMESNNOCTHSGGNVWTQHKBPR\
  \HMVUOYWLIAFIRIGDBOEBQLIGWARQHNLOISQKEPEIDVXXNETPAXNZGDX\
  \WWEYQCTIGONNGJVHSQGEATHSYGSDVVOAQCXLHSPQMDMETRTMDUXTEQQ\
  \JMFAEEAAIMEZREGIMUECICBXRVQRSMENNWTXTNSRNBPZHMRVRDYNECG\
  \SPMEAVTENXKEQKCTTHSPCMQQHSQGTXMFPBGLWQZRBOEIZHQHGRTOBSG\
  \TATTZRNFOSMLEDWESIWDRNAPBFOFHEGIXLFVOGUZLNUSRCRAZGZRTTA\
  \YFEHKHMCQNTZLENPUCKBAYCICUBNRPCXIWEYCSIMFPRUTPLXSYCBGCC\
  \UYCQJMWIEKGTUBRHVATTLEKVACBXQHGPDZEANNTJZTDRNSDTFEVPDXK\
  \TMVNAIQMUQNOHKKOAQMTBKOFSUTUXPRTMXBXNPCLRCEAEOIAWGGVVUS\
  \GIOEWLIQFOZKSPVMEBLOHLXDVCYSMGOPJEFCXMRUIGDXNCCRPMLCEWT\
  \PZMOQQSAWLPHPTDAWEYJOGQSOAVERCTNQQEAVTUGKLJAXMRTGTIEAFW\
  \PTZYIPKESMEAFCGJILSBPLDABNFVRJUXNGQSWIUIGWAAMLDRNNPDXGN\
  \PTTGLUHUOBMXSPQNDKBDBTEECLECGRDPTYBVRDATQHKQJMKEFROCLXN\
  \FKNSCWANNAHXTRGKCJTTRRUEMQZEAEIPAWEYPAJBBLHUEHMVUNFRPVM\
  \EDWEKMHRREOGZBDBROGCGANIUYIBNZQVXTGORUUCUTNBOEIZHEFWNBI\
  \GOZGTGWXNRHERBHPHGSIWXNPQMJVBCNEIDVVOAGLPONAPWYPXKEFKOC\
  \MQTRTIDZBNQKCPLTTNOBXMGLNRRDNNNQKDPLTLNSUTAXMNPTXMGEZKA\
  \EIKAGQ"


-------------------- Mutual Index Of Coincidence--------------------


spanishFreq :: [(Char, Double)]
spanishFreq =
  [ ('A',0.1253),('B',0.0142),('C',0.0468),('D',0.0586)
  , ('E',0.1368),('F',0.0069),('G',0.0101),('H',0.0070)
  , ('I',0.0625),('J',0.0044),('K',0.0002),('L',0.0497)
  , ('M',0.0315),('N',0.0671),('O',0.0868),('P',0.0251)
  , ('Q',0.0088),('R',0.0687),('S',0.0798),('T',0.0463)
  , ('U',0.0393),('V',0.0090),('W',0.0001),('X',0.0022)
  , ('Y',0.0090),('Z',0.0052)
  ]

relativeFreq :: Char -> String -> Double
relativeFreq c s = fromIntegral (length (filter (== c) s)) / fromIntegral (length s)

mutualIndex :: String -> Double
mutualIndex s = sum [ p * relativeFreq c s | (c,p) <- spanishFreq ]

micForShift :: String -> Shift -> Double
micForShift s b = mutualIndex (map (`shiftChar`b) s)

bestShift :: String -> Shift
bestShift s = snd $ maximumBy cmp results
  where
    results = [ (micForShift s b, b) | b <- map Shift [0..25] ]
    cmp (x,_) (y,_) = compare x y

shiftToKeyChar :: Shift -> Char
shiftToKeyChar b = shiftChar 'A' (invert b)

mutualIndexTest :: Int -> String -> String
mutualIndexTest k msg = map (shiftToKeyChar . bestShift) substrings
  where
    substrings = buildSubstrings k (msg)


----------------------------- Attack -----------------------------


attackVigenere :: String -> (String, String)
attackVigenere cipherText = (clave, textoDescifrado)
  where
    normText = normalize cipherText
    
    candidates = possibleLengths normText
    
    bestK = maximumBy compareIndexOfCoincidence candidates
    
    clave = mutualIndexTest bestK normText

    textoDescifrado = decrypt clave normText

    compareIndexOfCoincidence k1 k2 = compare (avgIC k1) (avgIC k2)
      where
        avgIC k = let result = indexOfCoincidenceTest k normText 
                  in sum result / fromIntegral (length result)
