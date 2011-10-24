

import System.Environment
import Data.Time
import System.Directory
import System.IO

data Activity = Finished ZonedTime ZonedTime String | Running ZonedTime deriving (Show, Read)

dispatch :: [(String, [String] -> IO ())]
dispatch = [
            ("start", start),
            ("stop", stop),
            ("report", report)
           ]

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

getDbDir :: IO FilePath
getDbDir = do
    home <- getHomeDirectory
    let db = home ++ "/.timelog" in do
        createDirectoryIfMissing True db
        return db

getDbFile :: String -> IO FilePath
getDbFile p = do
    db <- getDbDir
    return $ db ++ "/" ++ p

start :: [String] -> IO ()
start [project] = do
    w <- working
    case w of
        Nothing     -> do
            time <- getZonedTime
            file <- getDbFile $ "_" ++ project
            putStrLn $ "Work on project " ++ project ++ " started at " ++ show time
            writeFile file $ (show $ Running time) ++ "\n"
        Just open   -> do
            putStrLn $ "Focus! you are allready working on " ++ open

stop :: [String] -> IO ()
stop [note] = do
    w <- working
    case w of
        Just open   -> do
            inFile  <- getDbFile $ "_" ++ open
            outFile <- getDbFile open
            content <- parseFile inFile
            time    <- getZonedTime
            let (active:_)      = content
                Running from    = active
                updated         = (Finished from time note) in
                    appendFile outFile $ (show updated) ++ "\n"
            removeFile inFile
        Nothing     -> putStrLn $ "Working on nothing"


parseFile :: FilePath -> IO [Activity]
parseFile f = do
    content <- readFile f
    return $ map read (lines content)


report :: [String] -> IO ()
report _ = do
    w <- working
    case w of
        Just open   -> putStrLn $ "Working on " ++ open
        Nothing     -> putStrLn $ "Working on nothing"

working :: IO (Maybe String)
working = do
    db <- getDbDir
    content <- getDirectoryContents db
    let started = filter (\x -> (head x) == '_') content in
        if started == [] then return Nothing
        else return $ Just (tail $ head started)

