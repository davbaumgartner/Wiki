{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Wiki.Templates
Description : Base and default templates for Wiki
Copyright   : (c) David Baumgartner, 2014
License     : BSD3
Maintainer  : ch.davidbaumgartner@gmail.com
Stability   : stable
-}
module Wiki.Templates (PageTemplate(..),
                       appTemplate,
                       defaultMenu,
                       mkPage,
                       mkAnswer) where

  import           Happstack.Server
  import           Text.Blaze ((!))
  import qualified Text.Blaze.XHtml1.Strict as H
  import qualified Text.Blaze.XHtml1.Strict.Attributes as A
  import           Wiki.Translation
  import           Wiki.Utils

  -- |Datas to send to template
  data PageTemplate 
    = PageTemplate{  title   :: String,
                     sideBar :: Maybe H.Html, 
                     menu    :: H.Html, 
                     contentBody :: H.Html }

  -- |Basic template
  appTemplate :: 
                 Translater 
              -> PageTemplate 
              -> H.Html
  appTemplate translater page
    = let
        _tr = _text translater 
      in
        H.docTypeHtml $
          do H.head 
               $ do H.title 
                      $ _text noTranslation $ (++) "Wiki - " $ translater $ title page
                    H.script 
                      ! A.src   "https://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
                      ! A.type_ "application/javascript"
                      $ _empty
                    H.script
                      ! A.src   "/assets/js/kickstart.js" 
                      ! A.type_ "application/javascript"
                      $ _empty
                    H.link 
                      ! A.rel   "stylesheet" 
                      ! A.type_ "text/css" 
                      ! A.href  "http://fonts.googleapis.com/css?family=Roboto+Slab:300,400|Roboto&amp;subset=latin,greek"
                    H.link 
                      ! A.rel "stylesheet" 
                      ! A.type_ "text/css" 
                      ! A.href "/assets/css/kickstart.css"
                      ! A.media "all"
                    H.link 
                      ! A.rel "stylesheet" 
                      ! A.type_ "text/css" 
                      ! A.href "/assets/css/wiki.css"
                      ! A.media "all"
             H.body 
               $ H.div 
                   ! A.id "wrapper" 
                   $ do H.div 
                          ! A.id "header" 
                          $ do H.h1 $ _tr $ title page
                               menu page
                        H.div 
                          ! A.id "container" 
                          ! A.class_ "grid flex no-padding" 
                          $ case sideBar page of
                              Just side -> do H.div 
                                                ! A.id "content" 
                                                ! A.class_ "col_9" 
                                                $ contentBody page
                                              H.div 
                                                ! A.id "aside" 
                                                ! A.class_ "col_3" 
                                                $ side
                              Nothing -> H.div 
                                           ! A.id "content" 
                                           $ contentBody page

  -- |The default menu
  defaultMenu :: 
                 Translater 
              -> H.Html       -- ^ The items to add 
              -> H.Html
  defaultMenu translater menuAdd
    = let 
        _tr = _text translater 
      in
        H.ul 
          ! A.class_ "menu" 
          $ do H.li 
                 $ H.a 
                     ! A.href "/" 
                     $ _tr "Home"
               menuAdd
               H.li 
                 $ do H.a 
                        ! A.href "" 
                        $ do H.i 
                               ! A.class_ "icon-user" 
                               $ _empty
                             _tr "User"
                      H.ul 
                        $ do H.li 
                               $ H.a 
                                   ! A.href "/user/settings" 
                                   $ do H.i 
                                          ! A.class_ "icon-cog" 
                                          $ _empty
                                        _tr "Settings"
                             H.li 
                                $ H.a 
                                    ! A.href "/user/signout" 
                                    $ do H.i 
                                           ! A.class_ "icon-signout" 
                                           $ _empty
                                         _tr "Sign out"

  -- |Returns a page using the default configuration
  mkPage :: 
            Translater 
         -> String 
         -> H.Html 
         -> H.Html 
         -> H.Html
  mkPage translater title body menuAdd
    = appTemplate translater PageTemplate{  title = title,
                                            sideBar = Nothing,
                                            menu = defaultMenu translater menuAdd,
                                            contentBody = body }

  -- |Prepares an answer using 'mkPage'
  mkAnswer ::
             Translater 
          -> String 
          -> H.Html 
          -> H.Html 
          -> Response
  mkAnswer translater title body menuAdd
    = toResponse 
        $ mkPage translater title body menuAdd
