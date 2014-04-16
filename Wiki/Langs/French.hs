module Wiki.Langs.French where

  import           Wiki.Translation

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
       ("check your inputs", "Vérifiez vos entrées.")]

  translate :: 
               Identifier 
            -> Translation
  translate
    = translate' translation
