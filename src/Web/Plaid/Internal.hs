{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Plaid.Internal (deriveJSON') where

import Data.Aeson.TH
import Data.List (elemIndices)
import Language.Haskell.TH
import Text.Casing (fromHumps, toQuietSnake)

deriveJSON' :: Name -> Q [Dec]
deriveJSON' = deriveJSON
  (defaultOptions { fieldLabelModifier = toQuietSnake . fromHumps . (drop `bubbleCompose` fieldPrefixCount)
                  })

bubbleCompose f g x = f (g x) x

fieldPrefixCount :: String -> Int
fieldPrefixCount = ((+) 1) . head . tail . elemIndices '_'
