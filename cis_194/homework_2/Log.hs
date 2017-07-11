module Log where 

data MessegeType = Info | Warning | Error Int deriving (Show, Eq)

type TimeStamp = Int

data LogMessege = LogMessege MessegeType TimeStamp String | Uknown String deriving (Show, Eq)

data MessegeTree = Leaf | Node MessegeTree LogMessege MessegeTree deriving (Show, Eq)

newtype LogList = LogList [LogMessege]

{-Ls :: LogList
Ls = [LogMessege Info 6 "Completed armadillo processing",LogMessege Info 1 "Nothing to report",LogMessege (Error 99) 10 "Flange failed!"],LogMessege Info 4 "Ever
ything normal",LogMessege Info 11 "Initiating self-destruct sequence",LogMessege (Error 70) 3 "Way too many pickles",LogMessege (Error 65) 8 "Bad pickle-fl
ange interaction detected",LogMessege Warning 5 "Flange is due for a check-up",LogMessege Info 7 "Out for lunch, back in two time steps",LogMessege (Error
20) 2 "Too many pickles",LogMessege Info 9 "Back from lunch"]-}