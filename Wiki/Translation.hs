{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Wiki.Translation
Description : Main translation tools
Copyright   : (c) David Baumgartner, 2014
License     : BSD3
Maintainer  : ch.davidbaumgartner@gmail.com
Stability   : stable
-}
module Wiki.Translation (Identifier,
                         Translation,
                         Source,
                         Translater,
                         translate',
                         noTranslation) where

  import           Data.Maybe
  import           Data.Char

  -- |Identifier for translations
  type Identifier  = String
  -- |Translated string
  type Translation = String
  -- |Translation table
  type Source      = [(Identifier, Translation)]
  -- |Translater type
  type Translater  = (String -> String)

  -- |Base structure for a translater
  translate' :: 
                Source 
             -> Identifier 
             -> Translation
  translate' s i
    = fromMaybe i (lookup (map toLower i) s)

  -- |Blank translater: used when none is found
  noTranslation :: 
                   Translater
  noTranslation 
    = id
