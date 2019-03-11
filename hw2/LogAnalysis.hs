{-# OPTIONS_GHC -Wall #-}
module LogAnalysis (
    parseMessage,
    parse,
    insert,
    build,
    inOrder,
    whatWentWrong,
    getMessage,
    )
  where

import Log

parseMessage :: String -> LogMessage
parseMessage s = parseWords $ words s


parseWords :: [String] -> LogMessage
parseWords ("I":ts:rest) = LogMessage Info (read ts) (unwords rest)
parseWords ("W":ts:rest) = LogMessage Warning (read ts) (unwords rest)
parseWords ("E":s:ts:rest) = LogMessage (Error (read s)) (read ts) (unwords rest)
parseWords ws = Unknown (unwords ws)

parse :: String -> [LogMessage]
parse = map parseMessage . lines
  
insert :: LogMessage -> MessageTree -> MessageTree  
insert msg Leaf = Node Leaf msg Leaf
insert (Unknown _) tree = tree
insert msg (Node l (Unknown _) r) = (Node l msg r)
-- we can use there guards, but with this, it's this there no bindings for it's
-- pattern. So create guard from case-of
insert msg@(LogMessage _ ts _) (Node l cmsg@(LogMessage _ ct _) r) 
  = case ts `compare` ct of
    EQ -> Node l msg r
    GT -> Node l cmsg (insert msg r)
    LT -> Node (insert msg l) cmsg r

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l msg r) = (inOrder l) ++ msg:(inOrder r)

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ msg) = msg
getMessage (Unknown msg) = msg

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . (filter isRelevantError) . sort
  where sort = inOrder . build


isRelevantError :: LogMessage -> Bool
isRelevantError (LogMessage (Error s) _ _) = s >= 50
isRelevantError (LogMessage _ _ _) = False
isRelevantError (Unknown _) = False



