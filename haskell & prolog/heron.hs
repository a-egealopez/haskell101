import Data.List (sort)

heron :: (Ord a, Fractional a) => a -> a -> a -> Maybe a
heron a b c
  | not (isTriangle a b c) = Nothing
  | otherwise             = Just (newton f df x0)
  where
    s  = (a + b + c) / 2
    t  = s * (s - a) * (s - b) * (s - c)
    f x  = x * x - t
    df x = 2 * x
    x0 = max 1 (t / 2)

isTriangle :: (Ord a, Num a) => a -> a -> a -> Bool
isTriangle a b c = x > 0 && x + y > z
  where
    [x, y, z] = sort [a, b, c]

newton :: (Ord a, Fractional a) => (a -> a) -> (a -> a) -> a -> a
newton f df x0 = until isCloseEnough nextIteration x0
  where
    isCloseEnough x = abs (f x) < 1e-12
    nextIteration x = x - f x / df x


--------------------------------------------------------------------
----------------------------- Tests --------------------------------
--------------------------------------------------------------------

test_IsTriangle :: [Bool]
test_IsTriangle =
  [ isTriangle 3 4 5      -- rectangulo
  , isTriangle 5 5 5      -- equilatero
  , isTriangle 7 8 9      -- escaleno
  , isTriangle 0 4 5      -- lado nulo
  , isTriangle 1 2 3      -- NO
  , isTriangle 10 1 1     -- NO
  ]

test_Newton :: [Double]
test_Newton =
  [ newton (\x -> x*x - 2)  (2*) 1   -- √2
  , newton (\x -> x*x - 9)  (2*) 1   -- √9
  , newton (\x -> x*x - 25) (2*) 10  -- √25
  ]

-- Pruebas de heron
test_Heron :: [Maybe Double]
test_Heron =
  [ heron 3 4 5
  , heron 5 5 5
  , heron 7 8 9
  , heron 0 4 5
  , heron 1 2 3       
  , heron 10 1 1
  ]