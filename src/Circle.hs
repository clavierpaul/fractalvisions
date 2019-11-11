data Circle = 
     Circle { xCentre :: Double
            , yCentre :: Double
            , radius :: Double
     }

point :: Circle -> Double
point circle    | p < 0 = p + (2 * xEdge) + 1
                | otherwise = p + (2 * (xEdge - yEdge)) + 1
    where
        xEdge = 0
        yEdge = circle radius
        p = 1 - (circle radius)