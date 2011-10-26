module Report (report) where

import Storage
import Text.Printf

report :: [String] -> IO ()
report []  = do
    projects <- getProjectList
    putStr $ unlines projects

report [p] = do
    content <- getProjectContent p
    return ()

getProjectDuration :: String -> IO Float
getProjectDuration p = do
    content <- getProjectContent p
    return 0.1


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

