module Storage where

import System.Directory
import Data.Time


data Task = Active ZonedTime |
            Finished ZonedTime ZonedTime String |
            Logged ZonedTime Float String deriving (Read, Show)

from :: Task -> ZonedTime
from (Active t)         = t
from (Finished t _ _)   = t
from (Logged t _ _)     = t

getStorageDir :: IO FilePath
getStorageDir = do
    home <- getHomeDirectory
    let dir = home ++ "/.tog" in do
        createDirectoryIfMissing True dir
        return dir

getProjectFile :: String -> IO FilePath
getProjectFile p = do
    dir <- getStorageDir
    return $ dir ++ "/" ++ p

getActiveProjectFile :: String -> IO FilePath
getActiveProjectFile p = do
    dir <- getStorageDir
    return $ dir ++ "/_" ++ p


getActiveProjectContent' :: String -> IO Task
getActiveProjectContent' p = do
    file    <- getActiveProjectFile p
    content <- readFile file
    return $ read $ head $ lines content

getActiveProjectContent :: String -> IO (Maybe Task)
getActiveProjectContent p = do
    file        <- getActiveProjectFile p
    fileExist   <- doesFileExist file
    if fileExist
        then do
            content  <- readFile file
            return (Just (read . head $ lines content))
        else return Nothing

getProjectContent' :: String -> IO [Task]
getProjectContent' p = do
    file    <- getProjectFile p
    content <- readFile file
    return $ map read (lines content)

getProjectContent :: String -> IO (Maybe [Task])
getProjectContent p = do
    file        <- getProjectFile p
    fileExist   <- doesFileExist file
    if fileExist
        then do
            content  <- readFile file
            return (Just (map read (lines content)))
        else return Nothing

getActiveProject :: IO (Maybe String)
getActiveProject = do
    dir         <- getStorageDir
    contents    <- getDirectoryContents dir
    let active = filter (\x -> head x == '_') contents in
        if active == []
            then return Nothing
            else return $ Just (tail $ head active)

isProjectActive' :: String -> IO Bool
isProjectActive' p = do
    file        <- getActiveProjectFile p
    doesFileExist file

getProjectList :: IO [String]
getProjectList = do
    dir     <- getStorageDir
    content <- getDirectoryContents dir
    let l = filter (\x -> head x `elem` ['a'..'z']) content in
        return l

