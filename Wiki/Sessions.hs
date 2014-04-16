{-# LANGUAGE OverloadedStrings #-}
module Wiki.Sessions (UserSession(..),
                      SessionId,
                      emptySession,
                      writeSession,
                      readSession,
                      withSession) where

  import           Prelude
  import           Control.Applicative ((<$>))
  import           Control.Monad.IO.Class (liftIO, MonadIO)
  import           Happstack.Server
  import           Happstack.Server.Cookie
  import qualified Text.Blaze.XHtml1.Strict as H
  import           Data.Char (toLower)
  import           Data.UUID (toString)
  import           Data.UUID.V4
  import           System.Directory
  import           Wiki.Utils
  import           Wiki.Translation

  type SessionId
    = String
  data UserSession
    = UserSession{  userLang :: String,
                    userId   :: Maybe String }
                    deriving (Show, Read)

  emptySession ::
                  UserSession
  emptySession
    = UserSession{  userLang  = "en", 
                    userId    = Nothing }

  writeSession :: (Control.Monad.IO.Class.MonadIO m) =>
                  SessionId -> UserSession -> m ()
  writeSession session_id session
    = liftIO 
        $ writeFile ("/tmp/" ++ session_id) $ show session

  readSession :: (Control.Monad.IO.Class.MonadIO m) =>
                 SessionId 
              -> m UserSession
  readSession session_id
    = let
        f_path = "/tmp/" ++ session_id
      in
        liftIO
          $ doesFileExist f_path 
            >>= \exists ->
              if
                exists 
              then 
                read <$> readFile ("/tmp/" ++ session_id) 
              else
                return emptySession

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
