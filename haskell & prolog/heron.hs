-- Versión 1 (sin until, criterio x2 x1)

heron1 :: Double -> Double -> Double -> Maybe Double
heron1 a b c
  | not $ isTriangle a b c = Nothing
  | otherwise              = Just . newton f df $ x0
  where
    s = (a+b+c)/2
    t = s * (s-a) * (s-b) * (s-c)
    f x = x*x - t
    df x = 2*x
    x0 = max 1 $ t/2

    isTriangle :: Double -> Double -> Double -> Bool 
    isTriangle a b c = 
        a > 0 && b > 0 && c > 0 &&
        a+b>c && a+c>b && b+c>a

    newton :: (Double -> Double) -> (Double -> Double) -> Double -> Double
    newton f df x0 = selectNewton 1e-12 $ iterateNewton x0
        where            
            selectNewton :: Double -> [Double] -> Double
            selectNewton eps (x1:x2:xs)
                | abs (x2 - x1) < eps = x2
                | otherwise           = selectNewton eps (x2:xs)
            
            iterateNewton :: Double -> [Double]
            iterateNewton x0 = iterate (\x -> x - f x / df x) $ x0

-- Versión 1.5 (con until, criterio x2 x1)

heron15 :: Double -> Double -> Double -> Maybe Double
heron15 a b c
  | not $ isTriangle a b c = Nothing
  | otherwise              = Just . newton f df $ x0
  where
    s = (a+b+c)/2
    t = s * (s-a) * (s-b) * (s-c)
    f x = x*x - t
    df x = 2*x
    x0 = max 1 (t/2)

    isTriangle :: Double -> Double -> Double -> Bool 
    isTriangle a b c = 
        a > 0 && b > 0 && c > 0 &&
        a+b>c && a+c>b && b+c>a

    newton :: (Double -> Double) -> (Double -> Double) -> Double -> Double
    newton f df x0 = snd $ until isCloseEnough nextStep (x0, nextIteration x0)
      where
        nextIteration x = x - f x / df x
        
        isCloseEnough (x1, x2) = abs ( x2 - x1 ) < 1e-12
        
        nextStep (_, x2) = (x2, nextIteration x2)

-- Versión 2 (until, anidada,  criterio f x)

heron2 :: Double -> Double -> Double -> Maybe Double
heron2 a b c
  | not $ isTriangle a b c = Nothing
  | otherwise              = Just . newton f df $ x0
  where
    s = (a+b+c)/2
    t = s * (s-a) * (s-b) * (s-c)
    f x = x*x - t
    df x = 2*x
    x0 = max 1 $ t/2

    isTriangle :: Double -> Double -> Double -> Bool 
    isTriangle a b c = and [a > 0, b > 0, c > 0,
      a + b > c, a + c > b, b + c > a]

    newton :: (Double -> Double) -> (Double -> Double) -> Double -> Double
    newton f df x0 = until (isCloseEnough 1e-12) nextIteration x0
      where
        isCloseEnough :: Double -> Double -> Bool
        isCloseEnough eps x = abs (f x)  < eps
        
        nextIteration :: Double -> Double
        nextIteration = \x -> x - f x / df x

-- Versión 3 (until, no anidada, criterio f x)

isTriangle :: Double -> Double -> Double -> Bool 
isTriangle a b c = and [a > 0, b > 0, c > 0,
      a + b > c, a + c > b, b + c > a]

newton :: (Double -> Double) -> (Double -> Double) -> Double -> Double
newton f df x0 = until (isCloseEnough 1e-12) nextIteration x0
      where
        isCloseEnough :: Double -> Double -> Bool
        isCloseEnough eps x = abs (f x)  < eps
        
        nextIteration :: Double -> Double
        nextIteration = \x -> x - f x / df x

heron3 :: Double -> Double -> Double -> Maybe Double
heron3 a b c
  | not $ isTriangle a b c = Nothing
  | otherwise              = Just . newton f df $ x0
  where
    s = (a+b+c)/2
    t = s * (s-a) * (s-b) * (s-c)
    f x = x*x - t
    df x = 2*x
    x0 = max 1 $ t/2