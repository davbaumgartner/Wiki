{-|
Module      : Wiki.Utils
Description : Some utilitaries used everywhere
Copyright   : (c) David Baumgartner, 2014
License     : BSD3
Maintainer  : ch.davidbaumgartner@gmail.com
Stability   : stable
-}
module Wiki.Utils (_text,
                   _empty,
                   _method,
                   translaters,
                   getTranslater,
                   cookieExists) where

  import           Control.Monad (MonadPlus)
  import           Data.Maybe
  import           Data.Char
  import           Happstack.Server
  import qualified Text.Blaze.XHtml1.Strict as H
  import           Wiki.Translation
  import qualified Wiki.Langs.French as Tr_F
  import qualified Wiki.Langs.English as Tr_E

  -- |Translates a 'String' using the translater and output as HTML
  _text :: 
           Translater -- ^ The translater
        -> String     -- ^ The 'String' to translated and render as HTML
        -> H.Html     -- ^ The rendered HTML
  _text tr 
    = H.toHtml . tr

  -- |Returns an empty HTML block (hack)
  _empty ::
            H.Html
  _empty 
    = _text noTranslation []

  -- |Replacement for methodSP: handle only view that supports 'method'
  _method :: (Control.Monad.MonadPlus m,
              MatchMethod method, 
              ServerMonad m) =>
             method 
          -> m b -- ^ The method(s) that have to be supported
          -> m b
  _method m h 
    =    method m 
      >> nullDir 
      >> h

  -- |List of all supported translaters
  translaters ::
                 [(   String,  
                      Identifier    
                   -> Translation)]
  translaters
    = [("fr", Tr_F.translate),
       ("en", Tr_E.translate)]

  -- |Returns a translated by its identifier
  getTranslater ::
                   String         -- ^ Identifier
                -> Identifier     -- ^ Translater
                -> Translation
  getTranslater lang
    = fromMaybe noTranslation 
        $ lookup lang translaters

  -- |Returns True if the cookie 'name' exists, otherwise, False
  cookieExists :: (Monad m,
                   HasRqData m) => 
                  String          -- ^ Cookie's name
               -> m Bool
  cookieExists name 
    =     askRqEnv
      >>= \(_, _, cookies) ->
          return $ isJust $ lookup (map toLower name) cookies
