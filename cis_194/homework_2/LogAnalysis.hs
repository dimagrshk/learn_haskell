{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Data.String.Utils
import System.IO()

import Log

--data MessegeTree = Leaf | Node MessegeTree LogMessege MessegeTree deriving (Show)

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

parse :: String -> IO [LogMessege]
parse pathFile = do
    content <- readFile pathFile
    let fileLines = lines content
    let listOfMessege = map parseMessege fileLines
    return listOfMessege
    
singleton :: LogMessege -> MessegeTree   
singleton x = Node Leaf x Leaf  

timeSt :: LogMessege -> TimeStamp
timeSt (LogMessege (Error _ ) ts _) = ts
timeSt (LogMessege Warning ts _) = ts
timeSt (LogMessege Info ts _) = ts
timeSt (Uknown _) = 0

treeInsert :: LogMessege -> MessegeTree -> MessegeTree
treeInsert x Leaf = singleton x 
treeInsert x (Node left a right)
    | timeSt x == timeSt a = Node left x right
    | timeSt x < timeSt a = Node (treeInsert x left) a right
    | timeSt x > timeSt a = Node left a (treeInsert x right)
