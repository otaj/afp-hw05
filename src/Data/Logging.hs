module Data.Logging where

import           Data.List
import           Data.Time.Clock

data LogLevel = Debug | Info | Warning | Error | Fatal
              deriving (Show, Read, Eq, Ord, Enum, Bounded)

data EventSource = Internal { iesComponent :: String
                            , iesCallID    :: String }
                 | External { eesURI         :: String
                            , eesDescription :: String }
                 | Combined [EventSource]
                 | Unknown
                 deriving (Read, Eq, Ord)

data LogMessage = LogMessage
                { lmSource     :: EventSource
                , lmMessage    :: String
                , lmTimestamp  :: UTCTime
                , lmHiddenFlag :: Bool
                , lmLogLevel   :: LogLevel
                } deriving (Read, Eq)

data EventSourceMatcher = Exact EventSource
                        | With EventSource
                        | AnyInternal
                        | AnyExternal
                        | Any
                        | MatchAny [EventSourceMatcher]
                        | MatchAll [EventSourceMatcher]
                        deriving (Show, Read, Eq)

instance Show LogMessage where
    show (LogMessage {lmLogLevel=lev, lmSource=src, lmMessage=msg}) = "[" ++ show lev ++ "] " ++ show src ++ ": " ++ msg

instance Show EventSource where
    show (Internal x _) = "Internal[" ++ x ++ "]"
    show (External x _) = "External[" ++ x ++ "]"
    show Unknown        = "Unknown"
    show (Combined xs)  = "Combined[" ++ foldr (++) "" (intersperse "," $ map show xs) ++ "]"

instance Ord LogMessage where
    compare (LogMessage {lmHiddenFlag=hid1, lmLogLevel=lev1, lmTimestamp=ts1}) (LogMessage {lmHiddenFlag=hid2, lmLogLevel=lev2, lmTimestamp=ts2}) | hid1 /= hid2 = compare hid2 hid1
                                                                                                                                                  | lev1 /= lev2 = compare lev1 lev2
                                                                                                                                                  | ts1 /= ts2 = compare ts1 ts2


-- | Change log level operator
($=) :: LogMessage -> LogLevel -> LogMessage
($=) x level = x {lmLogLevel=level}


-- | EventSource "combinator"
(@@) :: EventSource -> EventSource -> EventSource
(@@) (Combined x) (Combined y) = Combined (x ++ y)
(@@) (Combined x) y            = Combined (x ++ [y])
(@@) x (Combined y)            = Combined (x : y)
(@@) x y                       = Combined [x, y]

-- | Matching EventSource with EventSourceMatcher operator
infixr 6 ~~
(~~) :: EventSourceMatcher -> EventSource -> Bool
(~~) (Exact x) y                                = x == y
(~~) (With x) (Combined y)                      = x `elem` y
(~~) AnyInternal (Internal _ _)                 = True
(~~) AnyInternal (Combined ((Internal _ _):xs)) = True
(~~) AnyInternal (Combined (x:xs))              = AnyInternal ~~ Combined xs
(~~) AnyExternal (External _ _)                 = True
(~~) AnyExternal (Combined ((External _ _):xs)) = True
(~~) AnyExternal (Combined (x:xs))              = AnyExternal ~~ Combined xs
(~~) Any _                                      = True
(~~) (MatchAny []) _                            = False
(~~) (MatchAny (x:xs)) y                        = (x ~~ y) || ((MatchAny xs) ~~ y)
(~~) (MatchAll []) _                            = True
(~~) (MatchAll (x:xs)) y                        = (x ~~ y) && ((MatchAll xs) ~~ y)
(~~) _ _                                        = False

-- | Specialized log list filter
logFilter :: EventSourceMatcher -> LogLevel -> Bool -> [LogMessage] -> [LogMessage]
logFilter matcher level hidden = filter (\(LogMessage {lmSource=src, lmHiddenFlag=hid, lmLogLevel=lev}) -> matcher ~~ src && hid == hidden && lev >= level)
