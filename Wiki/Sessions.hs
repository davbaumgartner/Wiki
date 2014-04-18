{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Wiki.Sessions
Description : User sessions
Copyright   : (c) David Baumgartner, 2014
License     : BSD3
Maintainer  : ch.davidbaumgartner@gmail.com
Stability   : stable
-}
module Wiki.Sessions (UserSession(..),
                      SessionId,
                      emptySession,
                      writeSession,
                      readSession,
                      withSession) where

  import           Prelude
  import           Control.Applicative ((<$>), (<*))
  import           Control.Monad.IO.Class (liftIO, MonadIO)
  import           Happstack.Server
  import           Happstack.Server.Cookie
  import qualified Text.Blaze.XHtml1.Strict as H
  import           Data.Char (toLower)
  import           Data.UUID (toString)
  import           Data.UUID.V4
  import           System.Directory
  import           Database.HDBC
  import           Database.HDBC.Sqlite3
  import           Wiki.Utils
  import           Wiki.Translation

  -- |Identifier for sessions (UUID)
  type SessionId
    = String
  -- |Session content description
  data UserSession
    = UserSession{  userLang :: String,
                    userId   :: Maybe String }
                    deriving (Show, Read)

  -- |Generates the path to the session
  generateSessionPath ::
                         String 
                      -> String 
  generateSessionPath 
    = (++) "/tmp/"

  -- |Empty session
  emptySession ::
                  UserSession
  emptySession
    = UserSession{  userLang  = "en", 
                    userId    = Nothing }

  -- |Writes data into a session (not working yet)
  writeSession :: (Control.Monad.IO.Class.MonadIO m) =>
                  SessionId -> UserSession -> m ()
  writeSession session_id session
    = let
        f_path = generateSessionPath session_id
      in
        liftIO 
          $     connectSqlite3 "private/sessions.db"
          >>= \db ->
                   run db "REPLACE INTO sessions (sid, sdatas) VALUES (?, ?)" [toSql session_id, toSql $ show session]
                >> commit db
                >> disconnect db
                >> return ()

  -- |Reads datas for a session
  readSession :: (Control.Monad.IO.Class.MonadIO m) =>
                 SessionId 
              -> m UserSession
  readSession session_id
    = liftIO
        $     connectSqlite3 "private/sessions.db" 
          >>= \db ->
                    quickQuery' db "SELECT sdatas FROM sessions WHERE sid = ? LIMIT 1" [toSql session_id] <* disconnect db
                >>= \datas ->
                      if
                        length datas > 0
                      then
                        return $ read (fromSql $ head $ head $ datas)
                      else
                           writeSession session_id emptySession
                        >> return emptySession

  -- |Used by views: setups main things (more than sessions btw), creates a new session if not exists.
  withSession ::
                 (   (SessionId,
                      UserSession,
                      Translater,
                      String -> H.Html)
                   -> ServerPart Response)
              -> ServerPart Response
  withSession code
    =    decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
      >> cookieExists "session" 
         >>= \t ->
             if
               t
             then
                   lookCookieValue "session"
               >>= \sessionId -> 
                   readSession sessionId
               >>= \session ->
                   let 
                     translater = getTranslater $ userLang session
                   in
                     code (sessionId, session, translater, _text translater)
             else
                   liftIO nextRandom
               >>= \uuid ->   
                   addCookie Session (mkCookie "session" (toString uuid))
               >>  writeSession (toString uuid) emptySession
               >>  let 
                     translater = getTranslater $ userLang emptySession
                   in
                     code (toString uuid, emptySession, translater, _text translater)
