module Storage where

import System.Directory
import Data.Time


data Task = Active ZonedTime |
            Finished ZonedTime ZonedTime String |
            Log Float String deriving (Read, Show)

getStorageDir :: IO FilePath
getStorageDir = do
    home <- getHomeDirectory
    let dir = home ++ "/.tog" in do
        createDirectoryIfMissing True dir
        return dir

getProjectFile :: Bool -> String -> IO FilePath
getProjectFile a p = do
    storage <- getStorageDir
    if a
        then return $ storage ++ "/_" ++ p
        else return $ storage ++ "/" ++ p

getActiveProject :: IO (Maybe String)
getActiveProject = do
    storage <- getStorageDir
    content <- getDirectoryContents storage
    let active = filter (\x -> head x == '_') content in
        if active == []
            then return Nothing
            else return $ Just (tail $ head active)

getProjectContent :: Bool -> String -> IO [Task]
getProjectContent active project = do
    projectFile <- getProjectFile active project
    fileExist   <- doesFileExist projectFile
    content     <- readFile projectFile
    return $ map read (lines content)

getActiveTask :: IO (Maybe (String, Task))
getActiveTask = do
    active <- getActiveProject
    case active of
        Just project    -> do
            (task:rest) <- getProjectContent True project
            return $ Just (project, task)
        Nothing         -> return Nothing

getProjectList :: IO [String]
getProjectList = do
    dir     <- getStorageDir
    content <- getDirectoryContents dir
    let l = filter (\x -> head x `elem` ['a'..'z']) content in
        return l

