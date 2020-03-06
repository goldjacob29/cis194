{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- ex 1

parseMessage :: String -> LogMessage
parseMessage s = 
    let split = words s
    in constructMessage split
        where 
            constructMessage [] = Unknown ""
            constructMessage  [x] = Unknown x
            constructMessage [x, y] = Unknown (x++y)
            constructMessage xs@(x:y:z:rest) 
                | x == "I" = LogMessage Info (read y::Int) (unwords (z:rest))
                | x == "W" = LogMessage Warning (read y::Int) (unwords (z:rest))
                | x == "E" = LogMessage (Error $ read y) (read z::Int) (unwords rest)
                | otherwise = Unknown (unwords xs)

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

-- ex 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf 
insert newMessage@(LogMessage _ t1 _) (Node left compMessage@(LogMessage _ t2 _) right)
    | (t1 <= t2) = (Node (insert newMessage left) compMessage right)
    | otherwise = (Node left compMessage (insert newMessage right))

-- ex 3
build :: [LogMessage] -> MessageTree
build ls = foldr insert Leaf ls

-- ex 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = inOrder left ++ [m] ++ inOrder right

-- ex 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = 
    let sorted = (inOrder . build) xs
    in filterLogMessages sorted
        where 
            filterLogMessages [] = []
            filterLogMessages (l:ls) =
                case l of
                    Unknown _ -> filterLogMessages ls
                    LogMessage mt _ s ->
                        case mt of
                            Error _ -> s:filterLogMessages ls
                            _ -> filterLogMessages ls

