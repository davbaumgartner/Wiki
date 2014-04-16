module Wiki.Langs.English where

  import           Wiki.Translation

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
       ("check your inputs", "Check your inputs.")]

  translate :: 
               Identifier 
            -> Translation
  translate
    = translate' translation
