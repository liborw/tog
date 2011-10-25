module Report (report) where

report :: [String] -> IO ()
report a = do
    putStrLn "This is a new report."
