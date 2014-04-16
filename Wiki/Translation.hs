{-# LANGUAGE OverloadedStrings #-}
module Wiki.Translation (Identifier,
                         Translation,
                         Source,
                         Translater,
                         translate',
                         noTranslation) where

  import           Data.Maybe
  import           Data.Char

  type Identifier  = String
  type Translation = String
  type Source      = [(Identifier, Translation)]
  type Translater  = (String -> String)

  translate' :: 
                Source 
             -> Identifier 
             -> Translation
  translate' s i
    = fromMaybe i (lookup (map toLower i) s)

  noTranslation :: 
                   Translater
  noTranslation 
    = id
