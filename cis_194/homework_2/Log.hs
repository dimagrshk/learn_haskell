

data MessegeType = Info | Warning | Error Int deriving (Show, Eq)

type TimeStamp = Int

data LogMessege = LogMessege MessegeType TimeStamp String | Uknown String deriving (Show, Eq)

