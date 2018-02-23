{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Plaid.Internal (deriveJSON') where

import Data.Aeson.TH
import Data.List (elemIndices)
import Language.Haskell.TH
import Text.Casing (fromHumps, toQuietSnake)

-- | deriveJSON that works with Obsidian-style record field naming
deriveJSON' :: Name -> Q [Dec]
deriveJSON' = deriveJSON
  (defaultOptions { fieldLabelModifier = toQuietSnake . fromHumps . stripRecordPrefix
                  })

-- XXX: This will runtime error out unless the field name is of the format `_{recordName}_{fieldName}`
-- Ideally we should figure out a way to enforce this constraint in the compiler.
stripRecordPrefix :: String -> String
stripRecordPrefix s = drop n s
  where n = 1 + (elemIndices '_' s !! 1)
