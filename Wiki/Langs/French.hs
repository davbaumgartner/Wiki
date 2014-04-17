{-|
Module      : Wiki.Langs.French
Description : Provide translations in french
Copyright   : (c) David Baumgartner, 2014
License     : BSD3
Maintainer  : ch.davidbaumgartner@gmail.com
Stability   : stable
-}
module Wiki.Langs.French where

  import           Wiki.Translation

  -- |Translation table
  translation ::
                 Source
  translation
    = [("home", "Accueil"), 
       ("user", "Utilisateur"),
       ("settings", "Préférences"), 
       ("sign out", "Déconnexion"),
       ("select language", "Sélectionnez la langue"),
       ("language", "Langue"), 
       ("french", "Françis"),
       ("english", "Anglais (english)"),
       ("save changes", "Enregistrer les modifications"),
       ("your request has been successfully threated",
        "Vôtre requête a été traitée avec succès."),
       ("check your inputs", "Vérifiez vos entrées."),
       ("not found", "Introuvable"),
       ("file not found", "Le fichier n'a pas pu être trouvé")]

  -- |Translater
  translate :: 
               Identifier 
            -> Translation
  translate
    = translate' translation
