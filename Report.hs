module Report where

import Storage
import Text.Printf
import Data.Time

report :: [String] -> IO ()
report []  = do
    projects    <- getProjectList
    totals      <- mapM projectSummary' projects
    putStrLn "------------------------"
    printf "Total           %5.1f h\n" (sum totals)
report [p] = do
    content <- getProjectContent p
    time <- getZonedTime
    return ()

projectSummary' :: String -> IO Float
projectSummary' p = do
    content <- getProjectContent' p
    active  <- isProjectActive' p
    time    <- getZonedTime
    let name    = iif active ("*" ++ p) p
        total   = sum $ map (duration time) content in do
            printf "%-15s %5.1f h\n" name total
            return total

getProjectDuration' :: String -> IO Float
getProjectDuration' p = do
    content <- getProjectContent p
    return 0.1

diffZonedTime :: ZonedTime -> ZonedTime -> NominalDiffTime
diffZonedTime a b = diffUTCTime (zonedTimeToUTC a) (zonedTimeToUTC b)

duration :: ZonedTime -> Task -> Float
duration _ (Logged d _)     = d
duration b (Active a)       = (realToFrac $ diffZonedTime b a) / 3600
duration _ (Finished a b _) = (realToFrac $ diffZonedTime b a) / 3600

iif :: Bool -> a -> a -> a
iif a b c
    | a         = b
    | otherwise = c

mark s = map aux
    where aux x = if x == s
                    then "*" ++ s
                    else x

