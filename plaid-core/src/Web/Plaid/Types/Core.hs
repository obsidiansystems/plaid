{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Plaid.Types.Core where

import Data.Aeson
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics

import Web.Plaid.Internal

data Config = Config
  { _config_env :: Environment
  , _config_clientId :: Text
  , _config_secret :: Text
  }
  deriving (Eq, Generic, Show)

data Environment
  = Sandbox
  | Development
  | Production
  deriving (Bounded, Enum, Eq, Generic, Show)

data Product
  = Product_Auth
  | Product_Identity
  | Product_Transactions
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

-- Plaid dates are returned in an ISO 8601 format (YYYY-MM-DD), so we just use
-- the Day type.
type Date = Day

type Money = Scientific

instance FromJSON Config where
  parseJSON = genericParseJSON fieldLabelMod

instance ToJSON Config where
  toJSON = genericToJSON fieldLabelMod

instance ToJSON Environment
instance FromJSON Environment
instance ToJSON Product
instance FromJSON Product
