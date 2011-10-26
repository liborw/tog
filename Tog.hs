import System.Environment
import Data.Time
import System.Directory
import System.IO

import Report
import Storage

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

log' :: [String] -> IO ()
log' [project, time, note] = do
    logActivity project (Log (read time) note)
    putStrLn $ "Logged " ++ time ++ " h to project " ++ project
log' [project, time] = log' [project, time, ""]

logActivity :: String -> Task -> IO ()
logActivity p a = do
    fileName <- getProjectFile False p
    appendFile fileName $ show a ++ "\n"

start :: [String] -> IO ()
start [project] = do
    r <- getActiveProject
    case r of
        Nothing     -> do
            time <- getZonedTime
            file <- getProjectFile True project
            putStrLn $ "Work on project " ++ project ++ " started at " ++ show time
            writeFile file $ show (Active time) ++ "\n"
        Just open   ->
            putStrLn $ "Focus! you are allready working on " ++ open
start [] = do
    cmd <- getProgName
    putStrLn $ "Usage: " ++ cmd ++ " start project_name"

stop :: [String] -> IO ()
stop [note] = do
    r <- getActiveProject
    case r of
        Just project -> do
            inFile   <- getProjectFile True project
            outFile  <- getProjectFile False project
            content  <- getProjectContent True project
            time     <- getZonedTime
            let (active:_)      = content
                Active from     = active
                updated         = (Finished from time note) in
                    appendFile outFile $ show updated ++ "\n"
            removeFile inFile
        Nothing     -> putStrLn "Working on nothing"
stop [] = stop [""]

status :: [String] -> IO ()
status _ = do
    r <- getActiveTask
    case r of
        Just (project, task) -> do
            time <- getZonedTime
            let Active from  = task
                duration     = diffZonedTime time from in
                    putStrLn $ "You are working on " ++ project ++ " for " ++ show duration
        Nothing     -> putStrLn "You are lazy bastard!"

diffZonedTime :: ZonedTime -> ZonedTime -> NominalDiffTime
diffZonedTime a b = diffUTCTime (zonedTimeToUTC a) (zonedTimeToUTC b)

diffTimeToHours :: NominalDiffTime -> Float
diffTimeToHours a = realToFrac a / 3600

