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
  import           Happstack.Server
  import qualified Wiki.Views as V
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

  -- |Runner
  main :: 
          IO ()
  main
    =    putStrLn "Starting server" 
      >> updateGlobalLogger rootLoggerName (setLevel INFO)
      >> simpleHTTP nullConf{port = 3000} routes