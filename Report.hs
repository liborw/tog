module Report (report) where

import Storage

report :: [String] -> IO ()
report a = do
    projects <- getProjectList
    putStr $ unlines projects


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

