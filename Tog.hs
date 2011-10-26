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
            ("report", report),
            ("help", help)
           ]


usages :: [(String, String)]
usages = [
         ("start", "<project>"),
         ("stop", "[note]"),
         ("log", "<project> <hours> [note]"),
         ("status", ""),
         ("report", "")
         ]

main = do
    args <- getArgs
    case args of
        []          -> help []
        (cmd:rest)  ->
            let (Just action) = lookup cmd dispatch in
                action args

printUsage :: String -> IO ()
printUsage cmd = do
    prog <- getProgName
    let (Just usage) = lookup cmd usages in
        putStrLn $ unwords ["Usage:", prog, cmd, usage]

help :: [String] -> IO ()
help _ = do
    prog <- getProgName
    putStrLn $ "Usage: " ++ prog ++ " <command> [args]\n"
    putStrLn "Commands:"
    putStr $ unlines $ map (\(x,y) -> "  " ++ unwords [x,y]) usages

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
start _ = printUsage "start"

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
stop _  = printUsage "stop"

status :: [String] -> IO ()
status [] = do
    r <- getActiveTask
    case r of
        Just (project, task) -> do
            time <- getZonedTime
            let Active from  = task
                duration     = diffZonedTime time from in
                    putStrLn $ "You are working on " ++ project ++ " for " ++ show duration
        Nothing     -> putStrLn "You are lazy bastard!"
status _ = printUsage "status"


diffZonedTime :: ZonedTime -> ZonedTime -> NominalDiffTime
diffZonedTime a b = diffUTCTime (zonedTimeToUTC a) (zonedTimeToUTC b)

diffTimeToHours :: NominalDiffTime -> Float
diffTimeToHours a = realToFrac a / 3600

