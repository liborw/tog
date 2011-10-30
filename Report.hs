module Report where

import Text.PrettyPrint.ANSI.Leijen
import System.Locale
import Storage
import Text.Printf
import Data.Time

report :: [String] -> IO ()
report []  = do
    projects    <- getProjectList
    putStrLn "Project       this day     weak    month    total"
    putStrLn "---------------------------------------------------"
    totals      <- mapM projectSummary' projects
    putStrLn "---------------------------------------------------"
    let   (today, weak, month, total) = foldl
                                        (\(a1,b1,c1,d1) (a2,b2,c2,d2)
                                            -> (a1 + a2, b1 + b2, c1 + c2, d1 + d2))
                                        (0, 0, 0, 0) totals in
        printf   "Total            %5.1f h  %5.1f h  %5.1f h  %5.1f h\n"
            today weak month total
report [p] = do
    content <- getProjectContent p
    time <- getZonedTime
    return ()

projectSummary' :: String -> IO (Float, Float, Float, Float)
projectSummary' p = do
    content <- getProjectContent' p
    active  <- isProjectActive' p
    time    <- getZonedTime
    let name                                = iif active ("*" ++ p) p
        (today, thisWeak, thisMonth, total) = timeSummary time content in do
            printf "%-15s  %5.1f h  %5.1f h  %5.1f h  %5.1f h\n"
                name today thisWeak thisMonth total
            return (today, thisWeak, thisMonth, total)

timeSummary :: ZonedTime -> [Task] -> (Float, Float, Float, Float)
timeSummary time tasks =
    let today     = sum $ map (duration time) (filter ((sameDay time) . from) tasks)
        thisWeak  = sum $ map (duration time) (filter ((sameWeak time) . from) tasks)
        thisMonth = sum $ map (duration time) (filter ((sameMonth time) . from) tasks)
        total     = sum $ map (duration time) tasks
        in (today, thisWeak, thisMonth, total)

getProjectDuration' :: String -> IO Float
getProjectDuration' p = do
    content <- getProjectContent p
    return 0.1

diffZonedTime :: ZonedTime -> ZonedTime -> NominalDiffTime
diffZonedTime a b = diffUTCTime (zonedTimeToUTC a) (zonedTimeToUTC b)

duration :: ZonedTime -> Task -> Float
duration _ (Logged _ d _)   = d
duration b (Active a)       = (realToFrac $ diffZonedTime b a) / 3600
duration _ (Finished a b _) = (realToFrac $ diffZonedTime b a) / 3600

iif :: Bool -> a -> a -> a
iif a b c
    | a         = b
    | otherwise = c

mark s = map aux
    where aux x = if x == s
                    then "*" ++ s
                    else x

formatTime' :: (FormatTime t) => t -> String
formatTime' = formatTime defaultTimeLocale "%R on %d %b %Y"

-- starting work on *project*
--   at 16:35 on 07 Feb 2011
printStart :: (FormatTime t) => String -> t -> IO ()
printStart p t = do
    putDoc $ nest 4 ( text "starting work on" <+> bold (text p)
             <$> text "at" <+> (text $ formatTime' t)) <> linebreak

-- working on *project*
--     form    16:35 on 07 Feb 2011
--     to now, 17:20 on 07 Feb 2011
--          => 1.5 h have elapsed
printStatus :: String -> ZonedTime -> Task -> IO ()
printStatus p t  task@(Active f) = do
    putDoc $ nest 4 ( text "working on" <+> bold (text p)
                    <$> text "from   " <+> (text $ formatTime' f)
                    <$> text "to now," <+> (text $ formatTime' t)
                    <$> indent 4 (red (text "==>"
                    <+> text (printf "%0.1f h" (duration t task))
                    <+> text "have elapsed")))
    putDoc linebreak

printStop :: String -> ZonedTime -> Task -> IO ()
printStop p t  task@(Active f) = do
    putDoc $ nest 4 ( text "worked on" <+> bold (text p)
                    <$> text "from   " <+> (text $ formatTime' f)
                    <$> text "to now," <+> (text $ formatTime' t)
                    <$> indent 4 (red (text "==>"
                    <+> text (printf "%0.1f h" (duration t task))
                    <+> text "elapsed")))
    putDoc linebreak

sameDay :: ZonedTime -> ZonedTime -> Bool
sameDay a b = (date a) == (date b)
    where date = formatTime defaultTimeLocale "%F"

sameWeak :: ZonedTime -> ZonedTime -> Bool
sameWeak a b = (weak a) == (weak b)
    where weak = formatTime defaultTimeLocale "%W-%Y"

sameMonth :: ZonedTime -> ZonedTime -> Bool
sameMonth a b = (month a) == (month b)
    where month = formatTime defaultTimeLocale "%m-%Y"
