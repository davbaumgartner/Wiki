{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Description : Runner for the Wiki
Copyright   : (c) David Baumgartner, 2014
License     : BSD3
Maintainer  : ch.davidbaumgartner@gmail.com
Stability   : stable
-}
module Main (main) where
  
  import           Control.Monad (msum, mzero)
  import           Database.HDBC
  import           Database.HDBC.Sqlite3
  import           Happstack.Server
  import qualified Wiki.Views as V
  import           System.Environment
  import           System.IO
  import           System.Log.Logger (updateGlobalLogger,
                                      rootLoggerName, 
                                      setLevel, 
                                      Priority(..))

  -- |List of the routes that have to be served by Happstack
  routes :: 
            ServerPart Response
  routes
    = msum 
        $ dir "assets" (serveDirectory DisableBrowsing [] "./assets") :
            V.routes

  -- |Arguments parser
  parseArgs :: [String] -> IO ()
  parseArgs ("-h":xs)
    =    putStrLn "Usage: wiki [-h] [-s] [-p]"
  parseArgs ("-s":xs)
    =    putStrLn "Initalizing databases"
      >> setupDB
      >> parseArgs xs
  parseArgs ("-p":xs)
    =     simpleHTTP nullConf{port = fromInteger $ read $ head xs } routes
  parseArgs _
    =     simpleHTTP nullConf{port = 3000} routes

  setupDB
    =     connectSqlite3 "private/sessions.db"
      >>= \db ->
               run db "CREATE TABLE sessions (sid VARCHAR(255), sdatas TEXT)" []
            >> commit db
            >> disconnect db
            >> return ()

  -- |Runner
  main :: 
          IO ()
  main
    =     updateGlobalLogger rootLoggerName (setLevel INFO)
      >>  getArgs
      >>= parseArgs