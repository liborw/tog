module Report where

import Storage
import Text.Printf
import Data.Time

report :: [String] -> IO ()
report []  = do
    projects    <- getProjectList
    active      <- getActiveProject
    case active of
        Just a  -> do
            let proj = mark a projects
            putStr $ unlines proj
        Nothing -> putStr $ unlines projects
report [p] = do
    content <- getProjectContent p
    time <- getZonedTime
    return ()

showProjectSummary' :: String -> IO Float
showProjectSummary' p = do
    content <- getProjectContent' p
    active  <- isProjectActive' p
    time    <- getZonedTime
    let name    = iif active ("*" ++ p) p
        total   = sum $ map (duration time) content in do
            printf "%s      %0.1f h" name total
            return total

getProjectDuration' :: String -> IO Float
getProjectDuration' p = do
    content <- getProjectContent p
    return 0.1

diffZonedTime :: ZonedTime -> ZonedTime -> NominalDiffTime
diffZonedTime a b = diffUTCTime (zonedTimeToUTC a) (zonedTimeToUTC b)

duration :: ZonedTime -> Task -> Float
duration _ (Log d _)        = d
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

-- report' :: [String] -> IO ()
-- report' [] = do
--     db <- getDbDir
--     content <- getDirectoryContents db
--     let filtered = filter (\x -> head x `elem` ['a'..'z']) content in
--         reportAux filtered

-- reportAux (x:xs) = do
--     total <- getTotal x
--     putStrLn $ x ++ "    " ++ show total ++ " h"
--     reportAux xs
-- reportAux [] = return ()
--
-- getTotal :: String -> IO Float
-- getTotal p = do
--     content     <- getProjectContent p
--     return $ total content
--
-- total :: [Activity] -> Float
-- total [] = 0.0
-- total (x:xs) = duration + total xs
--     where duration = case x of
--                         Finished from to _  -> diffTimeToHours (diffZonedTime to from)
--                         Log d _             -> d

