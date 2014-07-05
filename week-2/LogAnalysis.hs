{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- This one is not on me. Had to look it up on Google.
-- But learned read, words and unwords functions, which is cool.
-- Converting to a List of String, instead of parsing as list of characters,
-- which is what I tried to do originally, obviously makes much more sense.
parseMessage :: String -> LogMessage
parseMessage message =
  case words message of
    ("I" : ts : msg)       -> LogMessage Info (read ts) (unwords msg)
    ("W" : ts : msg)       -> LogMessage Warning (read ts) (unwords msg)
    ("E" : sev : ts : msg) -> LogMessage (Error (read sev)) (read ts) (unwords msg)
    (unknownMsg)           -> Unknown (unwords unknownMsg)

-- Yay for map, resulting in this one-liner.
parse :: String -> [LogMessage]
parse logData = map parseMessage (lines logData)

-- Not sure whether I like my solution here, but sticking with it for now
-- Pattern matches are also non-exhaustive. Whatever!
insert :: LogMessage -> MessageTree -> MessageTree
insert lm@(LogMessage _ ts _) tree =
  case tree of
    Node lt msg@(LogMessage _ ts1 _) rt ->
      if ts < ts1 then
        Node (insert lm lt) msg rt
      else
        Node lt msg (insert lm rt)
    Leaf -> Node Leaf lm Leaf
insert _ tree = tree

-- Now this is amazing. build is just an alias for foldR using insert.
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- inOrder is a fairly straightforward recursive implementation
inOrder :: MessageTree -> [LogMessage]
inOrder (Node lt lm rt) = inOrder lt ++ (lm : inOrder rt)
inOrder Leaf = []

-- Helper function to filter Error messages with severity > 50
-- so that we can use this to filter over [LogMessage] below
filterSeverity :: LogMessage -> Bool
filterSeverity (LogMessage (Error sev) _ _) = sev > 50
filterSeverity _ = False

-- Helper function to extract the message from a LogMessage
-- so that we can use this to map [LogMessage] -> [String] below
showMsg :: LogMessage -> String
showMsg (LogMessage _ _ msg) = msg
showMsg (Unknown msg) = msg

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgList = map showMsg (filter filterSeverity (inOrder (build msgList)))
