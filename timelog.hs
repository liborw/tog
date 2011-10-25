import System.Environment
import Data.Time
import System.Directory
import System.IO
import Text.Printf

data Activity = Finished ZonedTime ZonedTime String |
                Running ZonedTime |
                Log Float String deriving (Show, Read)

dispatch :: [(String, [String] -> IO ())]
dispatch = [
            ("start", start),
            ("stop", stop),
            ("status", status),
            ("log", log'),
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

getDbFile :: Bool -> String -> IO FilePath
getDbFile a p = do
    db <- getDbDir
    if a then return $ db ++ "/_" ++ p
    else return $ db ++ "/" ++ p

report :: [String] -> IO ()
report [] = do
    db <- getDbDir
    content <- getDirectoryContents db
    let filtered = filter (\x -> elem (head x)  ['a'..'z']) content in
        reportAux filtered

reportAux (x:xs) = do
    total <- getTotal x
    putStrLn $ x ++ "    " ++ (show total)

getTotal :: String -> IO Float
getTotal p = do
    content     <- getProjectContent p
    return $ total content

total :: [Activity] -> Float
total [] = 0.0
total (x:xs) = duration + (total xs)
    where duration = case x of
                        Finished from to _  -> diffTimeToHours (diffZonedTime to from)
                        Log d _             -> d


log' :: [String] -> IO ()
log' [project, time, note] = do
    logActivity project (Log (read time) note)
    putStrLn $ "Logged " ++ time ++ " h to project " ++ project
log' [project, time] = log' [project, time, ""]

start :: [String] -> IO ()
start [project] = do
    w <- working
    case w of
        Nothing     -> do
            time <- getZonedTime
            file <- getDbFile True project
            putStrLn $ "Work on project " ++ project ++ " started at " ++ show time
            writeFile file $ (show $ Running time) ++ "\n"
        Just open   -> do
            putStrLn $ "Focus! you are allready working on " ++ open

logActivity :: String -> Activity -> IO ()
logActivity p a = do
    fileName <- getDbFile False p
    appendFile fileName $ (show a) ++ "\n"

stop :: [String] -> IO ()
stop [note] = do
    w <- working
    case w of
        Just open   -> do
            inFile  <- getDbFile True open
            outFile <- getDbFile False open
            content <- parseFile inFile
            time    <- getZonedTime
            let (active:_)      = content
                Running from    = active
                updated         = (Finished from time note) in
                    appendFile outFile $ (show updated) ++ "\n"
            removeFile inFile
        Nothing     -> putStrLn $ "Working on nothing"
stop [] = stop [""]


parseFile :: FilePath -> IO [Activity]
parseFile f = do
    content <- readFile f
    return $ map read (lines content)

status :: [String] -> IO ()
status _ = do
    wa <- getWorkingActivity
    case wa of
        Just (p,a)  -> do
            time <- getZonedTime
            let Running from = a
                duration     = diffZonedTime time from in
                    putStrLn $ "You are working on " ++ p ++ " for " ++ show duration
        Nothing     -> putStrLn "You are lazy bastard!"

getProjectContent :: String -> IO [Activity]
getProjectContent p = do
    fileName    <- getDbFile False p
    parseFile fileName


getWorkingActivity :: IO (Maybe (String, Activity))
getWorkingActivity = do
    w <- working
    case w of
        Just open   -> do
            filePath <- getDbFile True open
            content <- parseFile filePath
            return $ Just (open, head content)
        Nothing     -> return Nothing

working :: IO (Maybe String)
working = do
    db <- getDbDir
    content <- getDirectoryContents db
    let started = filter (\x -> (head x) == '_') content in
        if started == [] then return Nothing
        else return $ Just (tail $ head started)

diffZonedTime :: ZonedTime -> ZonedTime -> NominalDiffTime
diffZonedTime a b = diffUTCTime (zonedTimeToUTC a) (zonedTimeToUTC b)

diffTimeToHours :: NominalDiffTime -> Float
diffTimeToHours a = (realToFrac a) / 3600

