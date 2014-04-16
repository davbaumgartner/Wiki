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

  _text :: 
           Translater 
        -> String 
        -> H.Html
  _text tr 
    = H.toHtml . tr

  _empty ::
            H.Html
  _empty 
    = _text noTranslation []

  _method :: (Control.Monad.MonadPlus m,
              MatchMethod method, 
              ServerMonad m) =>
             method 
          -> m b 
          -> m b
  _method m h 
    =    method m 
      >> nullDir 
      >> h

  translaters ::
                 [(   String, 
                      Identifier 
                   -> Translation)]
  translaters
    = [("fr", Tr_F.translate),
       ("en", Tr_E.translate)]

  getTranslater ::
                   String
                -> String
                -> String
  getTranslater lang
    = fromMaybe noTranslation 
        $ lookup lang translaters

  cookieExists :: (Monad m,
                   HasRqData m) => 
                  String 
               -> m Bool
  cookieExists name 
    =     askRqEnv
      >>= \(_, _, cookies) ->
          return $ isJust $ lookup (map toLower name) cookies
