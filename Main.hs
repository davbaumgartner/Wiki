{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
  
  import           Control.Monad (msum, mzero)
  import           Happstack.Server
  import qualified Wiki.Views as V
  import           System.IO
  import           System.Log.Logger (updateGlobalLogger,
                                      rootLoggerName, 
                                      setLevel, 
                                      Priority(..))

  routes :: 
            ServerPart Response
  routes
    = msum 
        $ dir "assets" (serveDirectory DisableBrowsing [] "./assets") :
            V.routes

  main :: 
          IO ()
  main
    =    putStrLn "Starting server" 
      >> updateGlobalLogger rootLoggerName (setLevel INFO)
      >> simpleHTTP nullConf{port = 3000} routes