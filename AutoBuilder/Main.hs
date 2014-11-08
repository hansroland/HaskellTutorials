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

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

-- | A data structure to store project path, cabal file
--   and running flag
data Config =  Config
   {cabalFile :: FilePath
   , confWorking :: Bool
   } deriving (Show, Read)


main = do
    args <- getArgs
    let dir = head args
    putStrLn $ "Watching directory: " ++ dir
    contents <- getDirectoryContents dir
    let contents' = filter filterCabal contents
    case contents' of
       (x : _) -> runThread x dir
       [] -> do
          putStrLn "No cabal file found!"
          putStrLn "Exiting ..."

runThread cabal dir = do
    -- first create a TVar with config data
    config <- newTVarIO $ Config cabal False
    n <- initINotify
    putStrLn "Press <Enter> to exit"
    print n
    wd <- addWatch n
             [Modify, CloseWrite, Create, Delete, MoveIn, MoveOut ]
             dir
             (eventHandler config)    -- pass th TVar to our event handler
    print wd
    getLine
    removeWatch wd
    killINotify n

-- | Event Handler for the Events we recieve from INotify
-- Filter out the events we are interested in
eventHandler :: TVar Config -> Event -> IO()
eventHandler conf ev@(Modified _ (Just fp))  = handleFilteredFile conf ev fp
eventHandler conf ev@(MovedIn  _ fp _)       = handleFilteredFile conf ev fp
eventHandler conf ev@(MovedOut _ fp _)       = handleFilteredFile conf ev fp
eventHandler conf ev@(Created  _ fp)         = handleFilteredFile conf ev fp
eventHandler conf ev@(Deleted  _ fp)         = handleFilteredFile conf ev fp
eventHandler _  _                            = return ()

-- | Filter Events by File Type
-- If the file is a *.hs
handleFilteredFile :: TVar Config -> Event -> FilePath -> IO()
handleFilteredFile conv ev fp = do
    if isMonitoredFile fp
        then print ev >> doWork conv fp
        else return ()



-- | Check that the file extension is .hs
isMonitoredFile :: FilePath -> Bool
isMonitoredFile fp = takeExtension fp `elem` [".hs"]

filterCabal fp = takeExtension fp == ".hs"

doWork :: TVar Config -> FilePath -> IO()
doWork conf fp = do
     config <- readTVarIO conf
     if confWorking config
        then do
          print "Already working!"
          return ()
        else do
          print "New work abailable!"
          atomically $ writeTVar conf (config {confWorking = True})
          return ()

