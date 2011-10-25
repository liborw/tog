module Storage where

import System.Directory


data Task = Active ZonedTime |
            Finished ZonedTime ZonedTime String |
            Log Float String deriving (Read, Show)

getStorageDir :: IO FilePath
    home <- getHomeDirectory
    createDirectoryIfMissing True storageDir
    return storageDir
        where storageDir = home ++ "/.tog"

getProjectFile :: Bool -> String -> IO FilePath
getProjectFile a p = do
    storage <- getStorageDir
    if a
        then return $ db ++ "/_" ++ p
        else return $ db ++ "/" ++ p

getActiveProject :: IO (Maybe String)
getActiveProject = do
    storage <- getStorageDir
    content <- getDirectoryContents storage
    let active = filter (\x -> head x == '_') content in
        if active == []
            then return Nothing
            else return $ Just (tail $ head active)

getProjectContent :: String -> IO (Maybe [Task])
getProjectContent project = do
    projectFile <- getProjectFile False project
    fileExist   <- doesFileExist projectFile
    if fileExist
        then do
            content <- readFile projectFile
            return $ map read (lines content)
        else return Nothing

getProjectContent' :: Bool -> String -> IO [Task]
getProjectContent' active project = do
    projectFile <- getProjectFile active project
    fileExist   <- doesFileExist projectFile
    content     <- readFile projectFile
    return $ map read (lines content)

getActiveTask :: IO (Maybe (String, Task))
getActiveTask = do
    active <- getActiveProject
    case active of
        Just project    -> do
            (task:rest) <- getProjectContent' True project
            return (project, task)
        NOthing         -> return Nothing

