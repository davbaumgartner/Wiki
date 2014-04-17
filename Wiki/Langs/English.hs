{-|
Module      : Wiki.Langs.French
Description : Provide translations in english
Copyright   : (c) David Baumgartner, 2014
License     : BSD3
Maintainer  : ch.davidbaumgartner@gmail.com
Stability   : stable
-}
module Wiki.Langs.English where

  import           Wiki.Translation

  -- |Translation table
  translation ::
                 Source
  translation
    = [("home", "Home"),
       ("user", "User"),
       ("settings", "Settings"),
       ("sign out", "Sign out"),
       ("select language", "Select language"),
       ("language", "Language"),
       ("french", "French (français)"),
       ("english", "English"),
       ("save changes", "Save changes"),
       ("your request has been successfully threated",
        "Your request has been successfully threated."),
       ("check your inputs", "Check your inputs."),
       ("not found", "Not found"),
       ("file not found", "File not found.")]

  -- |Translater
  translate :: 
               Identifier 
            -> Translation
  translate
    = translate' translation
