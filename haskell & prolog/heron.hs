heron :: Float -> Float -> Float -> Maybe Float
heron a b c
  | not $ isTriangle a b c = Nothing
  | otherwise              = Just . newton $ s * (s-a) * (s-b) * (s-c)
  where
    s = (a+b+c)/2

    isTriangle :: Float -> Float -> Float -> Bool 
    isTriangle a b c = a+b>c && a+c>b && b+c>a

    newton :: Float -> Float
    newton y = selectNewton 1e-6 $ iterateNewton y
        where 
            selectNewton :: Float -> [Float] -> Float
            selectNewton eps (x1:x2:xs)
                | abs (x2 - x1) < eps = x2
                | otherwise           = selectNewton eps (x2:xs)
            
            iterateNewton :: Float -> [Float]
            iterateNewton y = iterate (\x -> x - f x y / df x) $ max 1 (y/2)
                where 
                    f :: Float -> Float -> Float
                    f x y = x*x - y

                    df :: Float -> Float
                    df x = 2*x