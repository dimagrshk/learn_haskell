{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Data.String.Utils

--import Log
data MessegeType = Info | Warning | Error Int deriving (Show, Eq)

type TimeStamp = Int

data LogMessege = LogMessege MessegeType TimeStamp String | Uknown String deriving (Show, Eq)

parseMessege :: String -> LogMessege
parseMessege [] = Uknown ""
parseMessege (x:xs) 
    | x == 'I' = LogMessege Info (read code :: Int) inform
    | x == 'W' = LogMessege Warning (read code :: Int) inform
    | x == 'E' = LogMessege (Error (read code :: Int)) (read codeOfError :: Int) informOfError
    | otherwise = Uknown ""
    where inform = drop (2+(length code)) xs
          code = head (splitWs xs)
          codeOfError = head (drop 1 (splitWs xs))
          informOfError = drop (3 + (length code) + (length codeOfError)) xs

