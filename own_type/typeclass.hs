

{-class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)-}

data TrafficLights = Red | Yellow | Green

instance Eq TrafficLights where
    Red == Red = True
    Yellow == Yellow = True
    Green == Green = True
    _ == _ = False

instance Show TrafficLights where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light" 