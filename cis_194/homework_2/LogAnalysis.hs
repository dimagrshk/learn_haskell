{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessege :: String -> LogMessege
parseMessege [] = Unkown "S"
parseMessege (x:xs) 
    | x == "I" =    
    | x == "W" = 
    | x == "E" =  