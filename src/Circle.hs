data Circle = Circle { 
    xCentre :: Double, 
    yCentre :: Double,
    radius :: Double
}
-- heck
point :: Circle -> Double
point circle    | p < 0 = p + (2 * xEdge) + 1
                | otherwise = p + (2 * (xEdge - yEdge)) + 1
    where
        xEdge = 0
        yEdge = radius circle
        p = 1 - (radius circle)