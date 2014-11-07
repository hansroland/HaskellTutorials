-- ----------------------------------------------------------------------------
-- AutoBuilder: A simple automatic building system for Haskell
-- ----------------------------------------------------------------------------
--
-- See: http://voyageintech.wordpress.com/2014/11/05/continuous-builds-in-haskell-part-1/
--
-- ----------------------------------------------------------------------------

import System.Directory
import System.Environment
import System.INotify
import System.IO
import System.FilePath


main = do
    args <- getArgs
    let dir = head args
    putStrLn $ "Watching directory: " ++ dir
    n <- initINotify
    putStrLn "Press <Enter> to exit"
    print n
    wd <- addWatch n
             [Modify, CloseWrite, Create, Delete, MoveIn, MoveOut ]
             dir
             eventHandler
    print wd
    getLine
    removeWatch wd
    killINotify n

-- | Event Handler for the Events we recieve from INotify
-- Filter out the events we are interested in
eventHandler :: Event -> IO()
eventHandler ev@(Modified _ (Just fp))  = handleFilteredFile ev fp
eventHandler ev@(MovedIn  _ fp _)       = handleFilteredFile ev fp
eventHandler ev@(MovedOut _ fp _)       = handleFilteredFile ev fp
eventHandler ev@(Created  _ fp)         = handleFilteredFile ev fp
eventHandler ev@(Deleted  _ fp)         = handleFilteredFile ev fp
eventHandler _                         = return ()

-- | Filter Events by File Type
-- If the file is a *.hs
handleFilteredFile :: Event -> FilePath -> IO()
handleFilteredFile ev fp = do
    if isMonitoredFile fp
        then print ev >> doWork fp
        else return ()

-- | Check that the file extension is .hs
isMonitoredFile :: FilePath -> Bool
isMonitoredFile fp = (takeExtension fp) `elem` [".hs"]

doWork :: FilePath -> IO()
doWork fp = do
     putStrLn $ "Action of file: " ++ fp
     return ()

