import System.Environment
import Data.Time
import System.Directory
import System.IO
import Text.Printf

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
    putStrLn $ show args
    case args of
        []          -> help []
        (cmd:rest)  ->
            let (Just action) = lookup cmd dispatch in
                action rest

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
    fileName <- getProjectFile p
    appendFile fileName $ show a ++ "\n"

start :: [String] -> IO ()
start [project] = do
    r <- getActiveProject
    case r of
        Nothing     -> do
            time <- getZonedTime
            file <- getActiveProjectFile project
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
            inFile   <- getActiveProjectFile project
            outFile  <- getProjectFile project
            content  <- getActiveProjectContent' project
            time     <- getZonedTime
            let Active from     = content
                updated         = (Finished from time note) in
                    appendFile outFile $ show updated ++ "\n"
            removeFile inFile
        Nothing     -> putStrLn "Working on nothing"
stop [] = stop [""]
stop _  = printUsage "stop"

status :: [String] -> IO ()
status [] = do
    r <- getActiveProject
    case r of
        Just p  -> do
            task <- getActiveProjectContent' p
            time <- getZonedTime
            let d = duration task time in
                printf "You are working on %s for %0.1f h\n" p d
        Nothing -> putStrLn "You are lazy bastard!"
status _ = printUsage "status"

