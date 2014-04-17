{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Wiki.Views
Description : All the views and router
Copyright   : (c) David Baumgartner, 2014
License     : BSD3
Maintainer  : ch.davidbaumgartner@gmail.com
Stability   : stable
-}
module Wiki.Views (routes) where

  import           Control.Monad (msum)
  import           Control.Applicative ((<$>))
  import           Control.Monad.IO.Class (liftIO)
  import           Happstack.Server
  import           Happstack.Server.Cookie
  import           Data.Char (toLower)
  import           Data.UUID (toString)
  import           Data.UUID.V4
  import           Text.Blaze ((!))
  import           Text.Blaze.Internal (stringValue)
  import qualified Text.Blaze.XHtml1.Strict as H
  import qualified Text.Blaze.XHtml1.Strict.Attributes as A
  import qualified Wiki.Templates as T
  import           Wiki.Translation
  import           Wiki.Sessions
  import           Wiki.Utils

  -- |Displays the main page: /
  getMain :: 
             ServerPart Response
  getMain
    = withSession 
        $ \(sid, session, translater, _tr) ->
          ok 
            $ T.mkAnswer translater "Home"
                (do H.p 
                       $ _tr "He share of first to worse. Weddings and any opinions suitable smallest nay. My he houses or months settle remove ladies appear. Engrossed suffering supposing he recommend do eagerness. Commanded no of depending extremity recommend attention tolerably. Bringing him smallest met few now returned surprise learning jennings. Objection delivered eagerness he exquisite at do in. Warmly up he nearer mr merely me. "
                    H.p 
                       $ _tr "Remain valley who mrs uneasy remove wooded him you. Her questions favourite him concealed. We to wife face took he. The taste begin early old why since dried can first. Prepared as or humoured formerly. Evil mrs true get post. Express village evening prudent my as ye hundred forming. Thoughts she why not directly reserved packages you. Winter an silent favour of am tended mutual. ")
                _empty

  -- |HTML Form for settings
  settingsForm :: 
                  Translater
               -> H.Html
  settingsForm translater
    = let 
        _tr = _text translater 
      in
        H.form 
          ! A.method "post" 
          $ H.fieldset 
              $ do H.legend 
                     $ _tr "Language"
                   H.p 
                     $ do H.label 
                            ! A.for "lang" 
                            $ _tr "Select language"
                          H.br
                          H.select 
                            ! A.id "lang" 
                            ! A.name "lang" 
                            $ do H.option ! A.value "fr" $ _tr "French"
                                 H.option ! A.value "en" $ _tr "English"
                   H.p 
                     $ H.input 
                         ! A.type_ "submit" 
                         ! A.value (stringValue $ translater "Save changes")

  -- |Displays the form to change settings
  getSettings :: 
                 ServerPart Response
  getSettings
    = withSession 
        $ \(sid, session, translater, _tr) ->
          ok 
            $ T.mkAnswer translater "Settings" (settingsForm translater) _empty

  -- |Change settings
  postSettings :: 
                 ServerPartT IO Response
  postSettings
    = withSession 
        $ \(sid, session, translater, _tr) ->
              look "lang"
          >>= \lang ->
              if
                lang `elem` map fst translaters
              then
                   writeSession sid (session{userLang = lang})
                >> let 
                     translater = getTranslater lang
                   in
                     ok 
                       $ T.mkAnswer translater "Settings"
                           (do H.div 
                                 ! A.class_ "notice success" 
                                 $ H.i 
                                     ! A.class_ "icon-ok icon-large" 
                                     $ _text translater "Your request has been successfully threated"
                               settingsForm translater)
                           _empty
               else
                   badRequest
                     $ T.mkAnswer translater "Settings"
                         (do H.div 
                               ! A.class_ "notice error" 
                               $ H.i 
                                   ! A.class_ "icon-remove-sign icon-large" 
                                   $ _tr "Check your inputs"
                             settingsForm translater)
                         _empty

  -- |404 handler
  getNotFound :: 
                 ServerPart Response
  getNotFound
    = withSession 
        $ \(sid, session, translater, _tr) ->
          notFound 
            $ T.mkAnswer translater "Not found"
                (H.p 
                   $ _tr "File not found")
                _empty

  -- |Routes that have to be served
  routes ::
            [ServerPart Response]
  routes
    = [dir "user" $ dir "settings" $ _method GET getSettings,
       dir "user" $ dir "settings" $ _method POST postSettings,
       nullDir >> getMain,
       getNotFound]
